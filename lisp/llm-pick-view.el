;;; llm-pick-view.el --- Interactive main, model and compare views -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: DeepSeek:deepseek-v4-flash, GLM:glm-5.3-flash, Laguna:laguna-s-2.1
;; URL: https://github.com/OverbearingPearl/llm-pick
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Three stacked views sharing one keymap, no transient anywhere:
;;
;;   main view     *llm-pick*            every model, one section per
;;                                       group of naming sources
;;   model view    *llm-pick MODEL*      metadata of one model, its family,
;;                                       cheaper and dearer models
;;   compare view  *llm-pick compare M*  the neighbourhood of one model
;;                                       with the model view model as the
;;                                       baseline
;;
;; Keys in every view: n/p or j/k move entry by entry, f/b jump section
;; by section, RET opens the model view of the model under the cursor,
;; and q goes back one level.  The header line names the keys of the
;; view you are in.
;;
;; The records behind the views are collected once and cached on disk
;; for `llm-pick-view-cache-ttl' seconds (24 hours by default), so
;; repeated views open instantly and no source is downloaded twice.
;; `C-u M-x llm-pick' throws the cache away and fetches everything
;; again, all sources in one go.

;;; Code:

(require 'cl-lib)
(require 'llm-pick-core)
(require 'llm-pick-analyze)
(require 'llm-pick-query-run)
(require 'llm-pick-query-read)
(require 'llm-pick-source)
(require 'llm-pick-render-report)

(defgroup llm-pick-view nil
  "Interactive views of `llm-pick'."
  :group 'llm-pick
  :prefix "llm-pick-view-")

(defface llm-pick-view-title-face
  '((t :weight bold :height 1.1))
  "Face used to distinguish the title from the entries."
  :group 'llm-pick-view)

(defface llm-pick-view-header-face
  '((t :weight bold :inherit font-lock-keyword-face :overline t))
  "Face used to distinguish section headers from the entries."
  :group 'llm-pick-view)

(defface llm-pick-view-column-face
  '((t :weight bold :inherit font-lock-type-face))
  "Face used to distinguish the column legend from the entries."
  :group 'llm-pick-view)

(defcustom llm-pick-view-cache-ttl 3600
  "Seconds a collected set of records stays fresh on disk.
OpenRouter time-of-day overrides change a model's price every hour
\&, so one hour is the longest a cached record set can be
trusted.
3600 is one hour: within it, opening a view downloads nothing; past
it, the prices may belong to a different UTC override window."
  :type 'number)

(defcustom llm-pick-view-cache-dir
  (locate-user-emacs-file "llm-pick-cache")
  "Directory holding the fetched records between sessions."
  :type 'directory)

(defcustom llm-pick-view-cheaper-bands '(0.9 0.8 0.5)
  "Output price ratios naming the cheaper bands of a model view."
  :type '(repeat number))

(defcustom llm-pick-view-dearer-bands '(1.1 1.2 1.5)
  "Output price ratios naming the dearer bands of a model view."
  :type '(repeat number))

(defvar llm-pick-view--records nil
  "The records the views are showing, as `llm-pick-collect' returns them.
Buffer-local state is derived from this; a refresh replaces it.")

(defvar-local llm-pick-view--kind nil
  "Which view this buffer shows: main, model or compare.")

(defvar-local llm-pick-view--baseline nil
  "The record a model or compare view is built around.")

(defvar-local llm-pick-view--order nil
  "Field a main view sorts its models by, or nil for the canonical ID.")

(defvar llm-pick-view--reverse nil
  "Non-nil when the main view sorts in the flipped direction.
Numeric columns default to descending and the name column to
ascending; repeating the same sort key toggles this.")

;;; Collection cache

(defun llm-pick-view--cache-file ()
  "Return the file the records are cached in."
  (expand-file-name "records.el" llm-pick-view-cache-dir))

(defun llm-pick-view--cache-fresh-p ()
  "Return non-nil when the on-disk cache is younger than the TTL."
  (let* ((file (llm-pick-view--cache-file))
         (attrs (and (file-exists-p file) (file-attributes file))))
    (and attrs
         (< (float-time (time-subtract (current-time) (nth 5 attrs)))
            llm-pick-view-cache-ttl))))

(defun llm-pick-view-clear-cache ()
  "Delete the on-disk records cache and drop the in-memory one."
  (interactive)
  (let ((file (llm-pick-view--cache-file)))
    (when (file-exists-p file)
      (delete-file file))
    (setq llm-pick-view--records nil)
    (if (file-exists-p file)
        (message "Cache file %s could not be deleted" file)
      (message "Cache cleared%s" (if (file-exists-p file) "" "")))))

(defun llm-pick-view--save-cache (records)
  "Write RECORDS to the cache file, ignoring write errors."
  (condition-case nil
      (progn
        (make-directory llm-pick-view-cache-dir t)
        (with-temp-file (llm-pick-view--cache-file)
          (prin1 records (current-buffer))))
    (error nil)))

(defun llm-pick-view--models (&optional refresh)
  "Return the model records, fetching every source at most once a day.
A nil REFRESH reads the in-memory set, then the on-disk cache, and
only when both are missing or stale calls `llm-pick-source--collect',
which visits every registered source.  A non-nil REFRESH throws both
caches away first."
  (cond ((and (not refresh) llm-pick-view--records))
        ((and (not refresh) (llm-pick-view--cache-fresh-p))
         (setq llm-pick-view--records
               (with-temp-buffer
                 (insert-file-contents (llm-pick-view--cache-file))
                 (read (current-buffer)))))
        (t
         ;; `llm-pick-collect' lives in `llm-pick.el', which requires this
         ;; module, so the view calls the collector underneath it instead of
         ;; making the dependency circular.
         (setq llm-pick-view--records (llm-pick-source--collect :category (list nil "agentic" "coding" "reasoning" "multimodalGrounded" "knowledge" "multilingual" "instructionFollowing" "math" "intelligence")))
         (llm-pick-view--save-cache llm-pick-view--records)
         llm-pick-view--records)))

;;; Record helpers

(defun llm-pick-view--record-sources (record)
  "Return the sorted names of the sources that name RECORD.
This includes provider sources."
  (let (sources)
    (dolist (key (plist-get record :scores))
      (cl-pushnew (if (consp (car key)) (caar key) (car key)) sources))
    (dolist (pair (plist-get record :prices))
      (cl-pushnew (car pair) sources))
    (dolist (pair (plist-get record :providers))
      (cl-pushnew (car pair) sources))
    (sort sources (lambda (a b) (string< (symbol-name a) (symbol-name b))))))

(defun llm-pick-view--num (value &optional width)
  "Return VALUE as a short cell of WIDTH columns, or a dash."
  (format (format "%%%ds" (or width 9))
          (if (numberp value) (format "%g" value) "-")))

(defun llm-pick-view--line (record)
  "Return one summary line for RECORD.
Columns: model name, overall score, one BenchLM cell joining the 8
category scores with '/', one OpenRouter cell joining the 3 Artificial
Analysis scores with '/', one price cell per source joining the 3
price halves in, cached-in and out with '/' (e.g.
\"123.000/89.000/123.000\"), and the OpenRouter id.
Category scores render as exactly 3-character integers (%3.0f,
missing as \"---\"), joined with '/' with no padding, so the BenchLM
cell is 8*3+7=31 characters and the OpenRouter cell is 3*3+2=11
characters.  Price halves render as %7.3f (width 7); a nil price or
a 0 price from a non-openrouter source renders as \" --.---\" (the
zero/unknown marker), an openrouter 0 renders as \"  0.000\".  The
in, cached-in and out halves are joined with '/' into a 23-character
cell.  The price cells are for the default price source, named by the
variable llm-pick-core-default-price-source, followed by the
secondary price source, named by the variable
llm-pick-core-secondary-price-source.  Both names are resolved
dynamically here since those variables live in llm-pick.el.  The
name, score and id columns keep their fixed widths (the name
column is 50 characters wide, and longer display names are
truncated to 50 characters so that all columns stay aligned, and
the id column is left-aligned at 24 characters) so they
still line up with the header from llm-pick-view--insert-columns."
  (let* ((default-price (symbol-value 'llm-pick-core-default-price-source))
         (secondary-price (symbol-value 'llm-pick-core-secondary-price-source))
         (benchlm-categories '("agentic" "coding" "reasoning" "multimodalGrounded"
                               "knowledge" "multilingual" "instructionFollowing" "math"))
         (aa-categories '("intelligence" "coding" "agentic"))
         (bare-num (lambda (n)
                     (if (numberp n) (format "%3.0f" n) "---")))
         (price-cell (lambda (price source)
                       (cond
                        ((or (null price) (and (numberp price) (zerop price)
                                               (not (eq source 'openrouter))))
                         " --.---")
                        ((and (numberp price) (zerop price))
                         "  0.000")
                        (t
                         (format "%7.3f" price)))))
         (join (lambda (parts) (mapconcat #'identity parts "/")))
         (price-triple (lambda (source)
                         (concat
                          (funcall price-cell
                                   (llm-pick-core--price record source 'in)
                                   source)
                          "/"
                          (funcall price-cell
                                   (llm-pick-core--price record source 'cache)
                                   source)
                          "/"
                          (funcall price-cell
                                   (llm-pick-core--price record source 'out)
                                   source))))
         (name (truncate-string-to-width
                (or (plist-get record :display-name)
                    (llm-pick-core--field record 'name))
                50))
         (cells (append
                 (list (format "%-48s" name))
                 (list (format "%9s"
                               (funcall bare-num (llm-pick-core--field record 'score))))
                 (list (funcall join
                                (mapcar
                                 (lambda (cat)
                                   (funcall bare-num (llm-pick-core--score record 'benchlm cat)))
                                 benchlm-categories)))
                 (list (funcall join
                                (mapcar
                                 (lambda (cat)
                                   (funcall bare-num (llm-pick-core--score record 'openrouter cat)))
                                 aa-categories)))
                 (list (funcall price-triple default-price))
                 (list (funcall price-triple secondary-price))
                 (list (format "%-24s"
                               (or (cdr (assq 'openrouter (plist-get record :providers)))
                                   "-"))))))
    (mapconcat #'identity cells "  ")))

;;; Rendering primitives

(defun llm-pick-view--insert-header (title &optional note)
  "Insert a section header TITLE with an optional NOTE under it."
  (let ((start (point)))
    (insert (propertize title 'face 'llm-pick-view-header-face) "\n")
    (when note (insert note "\n"))
    (put-text-property start (point) 'llm-pick-header t)))

(defun llm-pick-view--insert-columns (&optional _columns)
  "Insert the column header line with `llm-pick-view-column-face'.

Category abbreviations in the header cells:
  BenchLM: Ag = Agentic, Co = Coding, Re = Reasoning, Mm = Multimodal,
           Kn = Knowledge, Ml = Multiling, IF = Instr-F, Ma = Math
  OpenRouter (OR): In = Intelligence, Co = Coding, Ag = Agentic.

Each score value is a fixed 3-character integer; missing values are
shown as \"---\".  Each price cell is a joined in/cache/out triple of
fixed 7-character components (23 chars total, 2 separator spaces);
missing values are shown as \"---\".

Column widths match those used by `llm-pick-view--line' so the header
aligns with the data rows: name %-48s (matching
`llm-pick-view--line''s truncation of display names to 50 characters),
score %9s, BenchLM joined cell 31 chars, OR joined cell 11 chars, each
price cell 23 chars, and the OpenRouter id left-aligned in %-24s.
Sorting: one key per column (see `llm-pick-view-mode-map'); repeating
the same key flips the direction."
  (let* ((head (concat (format "%-48s  %9s" "Model" "Score")))
         ;; The joined score cells aggregate category scores; list the
         ;; abbreviations so the compact columns are interpretable:
         ;; BenchLM has 8 categories (8*3+7=31 chars), OpenRouter-derived
         ;; scores have 3 (3*3+2=11 chars).  The full names exceed the
         ;; cell width, so the compact form is used; see the docstring
         ;; for the mapping.  Each score value is a fixed 3-character
         ;; integer (missing as "---").  Each price cell joins three
         ;; fixed 7-character in/cache/out values (3*7+2=23 chars).
         (head (concat head "  " (format "%-31s"
                                         "BenchLM(Ag/Co/Re/Mm/Kn/Ml/IF/Ma)")))
         (head (concat head "  " (format "%-11s" "OR(In/Co/Ag)")))
         (head (concat head "  " (format "%-23s" "BenchLM $/M in/ca/out")))
         (head (concat head "  " (format "%-23s" "OpenRT $/M in/ca/out")))
         (head (concat head "  " (format "%-24s" "OpenRouter id")))
         (start (point)))
    (insert (propertize head
                        'face 'llm-pick-view-column-face) "\n")
    (put-text-property start (point) 'llm-pick-header t)))

(defun llm-pick-view--insert-entry (record)
  "Insert one selectable line for RECORD."
  (insert (propertize (llm-pick-view--line record)
                      'llm-pick-record record)
          "\n"))

(defun llm-pick-view--insert-entries (records)
  "Insert every RECORD of RECORDS as a selectable line."
  (dolist (record records) (llm-pick-view--insert-entry record)))

(defun llm-pick-view--render (kind title body)
  "Prepare the current buffer as a view of KIND named TITLE.
BODY is a function inserting the buffer content."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize title 'face 'llm-pick-view-title-face) "\n"
            (propertize (make-string (length title) ?=) 'face 'font-lock-comment-face)
            "\n")
    (funcall body)
    (goto-char (point-min))
    (setq llm-pick-view--kind kind
          header-line-format (llm-pick-view--header-line))
    (setq buffer-read-only t)))

(defun llm-pick-view--header-line ()
  "Return the key hint for the header line of the main view."
  (let* ((word-hint
          (lambda (word)
            ;; Render a word like "[N]ame" or "Multi[m]odal": characters
            ;; wrapped in square brackets are displayed in bold with
            ;; underline (keeping their original case), the rest with the
            ;; default face.
            (let ((pos 0)
                  (parts nil))
              (while (< pos (length word))
                (if (eq (aref word pos) ?\[)
                    (let ((close (string-search "]" word pos)))
                      (if close
                          (progn
                            (push (propertize (substring word (1+ pos) close)
                                              'face '(:underline t
                                                      :weight bold))
                                  parts)
                            (setq pos (1+ close)))
                        (push (substring word pos) parts)
                        (setq pos (length word))))
                  (let ((next (string-search "[" word pos)))
                    (push (substring word pos (or next (length word))) parts)
                    (setq pos (or next (length word))))))
              (string-join (nreverse parts)))))
         (group-words
          (list
           ;; Score
           (list "[S]core")
           ;; BenchLM capability keys
           (list "[A]gentic" "[C]oding" "[R]easoning" "Multi[m]odal"
                 "[K]nowledge" "Multi[l]ing" "In[s]tructionF" "M[a]th")
           ;; OpenRouter capability keys
           (list "Intell[i]gence" "C[o]ding" "Ag[e]ntic")
           ;; BenchLM price keys
           (list "[I]nput" "Ca[c]he" "[O]utput")
           ;; OpenRouter price keys
           (list "Inp[u]t" "Cac[h]e" "Outpu[t]")))
         (word-sep "/")
         (group-sep "  ")
         (plain-groups
          (concat "n/p/j/k (move)" group-sep "f/b (section)"))
         (sort-hint
          (concat
           plain-groups
           group-sep
           (string-join
            (mapcar
             (lambda (words)
               (string-join
                (mapcar (lambda (w) (funcall word-hint w)) words)
                word-sep))
             group-words)
            group-sep))))
    sort-hint))

;;; Movement

(defun llm-pick-view--entry-at-point ()
  "Return the record under the cursor, or nil."
  (get-text-property (point) 'llm-pick-record))

(defun llm-pick-view-next (&optional count)
  "Move down COUNT entries, to the next model line."
  (interactive "p")
  (dotimes (_ (or count 1))
    (forward-line 1)
    (while (and (not (eobp)) (not (llm-pick-view--entry-at-point)))
      (forward-line 1))))

(defun llm-pick-view-previous (&optional count)
  "Move up COUNT entries, to the previous model line."
  (interactive "p")
  (dotimes (_ (or count 1))
    (forward-line -1)
    (while (and (not (bobp)) (not (llm-pick-view--entry-at-point)))
      (forward-line -1))))

(defun llm-pick-view-forward-section ()
  "Move to the next section header."
  (interactive)
  (forward-line 1)
  (while (and (not (eobp))
              (not (get-text-property (point) 'llm-pick-header)))
    (forward-line 1)))

(defun llm-pick-view-backward-section ()
  "Move to the previous section header."
  (interactive)
  (forward-line -1)
  (while (and (not (bobp))
              (not (get-text-property (point) 'llm-pick-header)))
    (forward-line -1)))

;;; The mode and its keymap

(defvar llm-pick-view-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "n" 'llm-pick-view-next)
    (define-key map "p" 'llm-pick-view-previous)
    (define-key map "j" 'llm-pick-view-next)
    (define-key map "k" 'llm-pick-view-previous)
    (define-key map "f" 'llm-pick-view-forward-section)
    (define-key map "b" 'llm-pick-view-backward-section)
    (define-key map (kbd "RET") 'llm-pick-view-ret)
    (define-key map "q" 'llm-pick-view-quit)
    (define-key map "g" 'llm-pick-view-refresh)
    ;; One key per sortable column; a repeat of the same key flips
    ;; the direction (see `llm-pick-view-sort-toggle').
    ;; N = name, S = overall score, A/C/R/m/K/l/s/a = BenchLM category,
    ;; i/o/e = OpenRouter category, I/c/O = BenchLM prices,
    ;; u/h/t = OpenRouter prices.
    (dolist (spec '(("N" . name)
                    ("S" . score)
                    ("A" . (benchlm . "agentic"))
                    ("C" . (benchlm . "coding"))
                    ("R" . (benchlm . "reasoning"))
                    ("m" . (benchlm . "multimodalGrounded"))
                    ("K" . (benchlm . "knowledge"))
                    ("l" . (benchlm . "multilingual"))
                    ("s" . (benchlm . "instructionFollowing"))
                    ("a" . (benchlm . "math"))
                    ("i" . (openrouter . "intelligence"))
                    ("o" . (openrouter . "coding"))
                    ("e" . (openrouter . "agentic"))
                    ("I" . (benchlm . in))
                    ("c" . (benchlm . cache))
                    ("O" . (benchlm . out))
                    ("u" . (openrouter . in))
                    ("h" . (openrouter . cache))
                    ("t" . (openrouter . out))))
      (define-key map (car spec)
                  (lambda () (interactive)
                    (llm-pick-view-sort-toggle (cdr spec)))))
    map)
  "Keymap of `llm-pick-view-mode'.")

(defvar llm-pick-view--jump-target nil
  "Record the view returns to with `q', when it is not the parent.")

(define-derived-mode llm-pick-view-mode special-mode "llm-pick-view"
  "Major mode of the `llm-pick' main, model and compare views."
  (setq-local truncate-lines t))

;;; Main view

(defun llm-pick-view--sort (records)
  "Return RECORDS in the order of the main view.
Numeric columns sort strongest first and the name column ascending;
`llm-pick-view--reverse', set by repeating the same sort key,
flips the direction.  Missing numeric keys rank last either way.
Keys are normalized before comparison: the name branch coerces to
strings and the numeric branch coerces to numbers (missing keys
become \"\" / -1), avoiding mixed-type comparison errors."
  (sort (copy-sequence records)
        (lambda (a b)
          (let* ((rawa (llm-pick-view--sort-key a))
                 (rawb (llm-pick-view--sort-key b))
                 (name-p (or (null llm-pick-view--order)
                             (eq llm-pick-view--order 'name)))
                 (ka (if name-p
                         (if (stringp rawa) rawa (format "%s" (or rawa "")))
                       (if (numberp rawa) rawa -1)))
                 (kb (if name-p
                         (if (stringp rawb) rawb (format "%s" (or rawb "")))
                       (if (numberp rawb) rawb -1)))
                 (less (if name-p
                           (string< ka kb)
                         (> ka kb))))
            (if llm-pick-view--reverse (not less) less)))))

(defun llm-pick-view--key-face ()
  "Face for key hints: red, underlined, and bold."
  (let ((face (make-face 'llm-pick-view-key-face)))
    (set-face-foreground face "red")
    (set-face-underline face t)
    (set-face-bold face t)
    face))

(defun llm-pick-view--key-hint (prefix key suffix)
  "Concatenate PREFIX, KEY, and SUFFIX into a hint fragment.
KEY is given the keybinding face."  (concat prefix
          (propertize key 'face (llm-pick-view--key-face))
          suffix))

(defun llm-pick-view--sort-key (record)
  "Return the sort key of RECORD for the current `llm-pick-view--order'.
The order value is either a plain field symbol or a cons cell.
For a category score column (SOURCE . CATEGORY) the key is the
result of `llm-pick-core--score'; for a price column (SOURCE .
PART) where PART is `in', `cache' or `out' the key is the result
of `llm-pick-core--price'.  The `name' order and the nil order
read the display name via `llm-pick-core--field'.  A nil or
`unknown' key is treated as the worst possible value: -1 for
numeric keys and \"\" for name keys."
  (let ((order llm-pick-view--order)
        key)
    (cond
     ((or (not order) (eq order 'name))
      (setq key (llm-pick-core--field record 'name)))
     ((symbolp order)
      (setq key (llm-pick-core--field record order)))
     ((and (consp order)
           (memq (cdr order) '(in cache out)))
      (setq key (llm-pick-core--price record
                                      (car order) (cdr order))))
     ((consp order)
      (setq key (llm-pick-core--score record
                                      (car order) (cdr order)))))
    (if (or (eq key 'unknown) (null key))
        (if (stringp key) "" -1)
      key)))

(defun llm-pick-view-sort-toggle (order)
  "Re-render the main view sorted by ORDER.
The first press sorts the column in its default direction
\\(numeric columns strongest first, the name column ascending);
pressing the same key again flips it."
  (if (equal llm-pick-view--order order)
      (setq llm-pick-view--reverse (not llm-pick-view--reverse))
    (setq llm-pick-view--order order
          llm-pick-view--reverse nil))
  (llm-pick-view-main))

(defun llm-pick-view--main-groups (records)
  "Return RECORDS grouped by the exact set of sources naming them.
The result is an alist ((SET-NAME . MODELS)...); a model named by
more than one source lands in the shared group first, then one group
per source holds what only that source lists."
  (let ((shared (make-hash-table :test 'equal))
        (only (make-hash-table :test 'equal))
        shared-key)
    (dolist (record records)
      (let ((sources (llm-pick-view--record-sources record)))
        (when sources
          (if (> (length sources) 1)
              (progn
                (setq shared-key (mapcar #'symbol-name sources))
                (puthash shared-key
                         (append (gethash shared-key shared) (list record))
                         shared))
            (let ((key (list (mapcar #'symbol-name sources))))
              (puthash key
                       (append (gethash key only) (list record))
                       only))))))
    (append
     (when shared-key
       (list (cons "shared by several sources"
                   (gethash shared-key shared))))
     (let (groups)
       (maphash (lambda (key models)
                  (push (cons (format "only in %s" (mapconcat #'identity (car key) ", "))
                              models)
                        groups))
                only)
       (sort groups (lambda (a b) (string< (car a) (car b))))))))

(defun llm-pick-view-main (&optional refresh)
  "Show every model of every source in the *llm-pick* buffer.
With REFRESH non-nil, fetch everything again, ignoring the cache.
The models are grouped in sections: the canonical IDs several sources
share first, then one section per source for what it lists alone."
  (interactive "P")
  (let* ((records (llm-pick-view--models refresh))
         (groups (llm-pick-view--main-groups (llm-pick-view--sort records))))
    (pop-to-buffer (get-buffer-create "*llm-pick*"))
    (llm-pick-view-mode)
    (llm-pick-view--render
     'main
     (format "llm-pick: %d models" (length records))
     (lambda ()
       (llm-pick-view--insert-columns '("Model" "Score" "In $/M" "Value"))
       (insert "\n")
       (dolist (group groups)
         (llm-pick-view--insert-header (car group))
         (llm-pick-view--insert-entries (cdr group)))))))

;;; Main view commands

(defun llm-pick-view-sort (field)
  "Re-render the main view sorted by FIELD, strongest first."
  (setq llm-pick-view--order field)
  (llm-pick-view-main))

(defun llm-pick-view-refresh ()
  "Throw the cache away, fetch every source again and re-render."
  (interactive)
  (setq llm-pick-view--records nil)
  (llm-pick-view-main 'refresh))

;;; Model view

(defun llm-pick-view--metadata (record)
  "Return the metadata block of RECORD as text."
  (let ((lines
         (list
          (format "ID:        %s" (llm-pick-core--field record 'name))
          (format "Name:      %s" (or (plist-get record :display-name) "-"))
          (format "Vendor:    %s" (or (llm-pick-core--field record 'vendor) "-"))
          (format "Family:    %s" (or (llm-pick-core--field record 'family) "-"))
          (format "Scope:     %s" (llm-pick-core--field record 'scope)))))
    (dolist (key (plist-get record :scores))
      (setq lines
            (append lines
                    (list
                     (if (consp (car key))
                         (format "Score %s/%s: %s" (caar key) (cdar key)
                                 (cdr key))
                       (format "Score %s: %s" (car key) (cdr key)))))))
    (dolist (pair (plist-get record :prices))
      (setq lines
            (append lines
                    (list (format "Price %s: in $%s  out $%s per M"
                                  (car pair)
                                  (llm-pick-view--num (cdr (assq :in (cdr pair))))
                                  (llm-pick-view--num (cdr (assq :out (cdr pair)))))))))
    (dolist (provider (plist-get record :providers))
      (setq lines
            (append lines
                    (list (format "Provider %s: %s" (car provider)
                                  (cdr provider))))))
    (mapconcat #'identity lines "\n")))

(defun llm-pick-view--ratio (model baseline)
  "Return MODEL's output price as a multiple of BASELINE's, or nil."
  (llm-pick-analyze--benchmark-ratio model baseline))

(defun llm-pick-view--bands (records baseline bounds below)
  "Return RECORDS in price-ratio bands around BASELINE.
BOUNDS lists the band edges; BELOW selects the cheaper side, so the
bands run under 1 on the cheap side and over 1 on the dear side.  The
result is an alist ((TITLE . MODELS)...), empty bands left out."
  (let* ((ratios (mapcar (lambda (model)
                           (cons model (llm-pick-view--ratio model baseline)))
                         records))
         (kept (cl-remove-if-not
                (lambda (pair)
                  (and (cdr pair)
                       (if below (< (cdr pair) 1) (> (cdr pair) 1))))
                ratios))
         (edges (if below (reverse bounds) bounds))
         result)
    (let ((previous (if below nil 1)))
      (dolist (edge edges)
        (let* ((band (cl-remove-if-not
                      (lambda (pair)
                        (and (cdr pair)
                             (if below
                                 (and (> (cdr pair) edge)
                                      (<= (cdr pair) (or previous edge)))
                               (and (> (cdr pair) previous)
                                    (<= (cdr pair) edge)))))
                      kept))
               (title (if below
                          (format "%.1fx - %.1fx the price" edge (or previous edge))
                        (format "%.1fx - %.1fx the price" previous edge))))
          (setq previous edge)
          (when band
            (push (cons title
                        (mapcar #'car
                                (sort band
                                      (lambda (a b) (< (cdr a) (cdr b))))))
                  result)))))
    (nreverse result)))

(defun llm-pick-view-model (record)
  "Show the model view of RECORD in its own buffer."
  (let* ((name (llm-pick-core--field record 'name))
         (buffer (get-buffer-create (format "*llm-pick %s*" name)))
         (records (llm-pick-view--models))
         (family (llm-pick-core--field record 'family))
         (kin (and family
                   (cl-remove-if-not
                    (lambda (other)
                      (and (not (equal other record))
                           (equal (llm-pick-core--field other 'family)
                                  family)))
                    records)))
         (cheaper (llm-pick-view--bands records record
                                        llm-pick-view-cheaper-bands t))
         (dearer (llm-pick-view--bands records record
                                       llm-pick-view-dearer-bands nil)))
    (pop-to-buffer buffer)
    (llm-pick-view-mode)
    (setq llm-pick-view--baseline record)
    (llm-pick-view--render
     'model (format "llm-pick model: %s" name)
     (lambda ()
       (insert (llm-pick-view--metadata record) "\n")
       (llm-pick-view--insert-header "Metadata" "Everything the sources say about this model.")
       (insert "")
       (llm-pick-view--insert-header
        (format "Same family (%s)" (or family "unknown"))
        "Other models of the same vendor family.")
       (if kin (llm-pick-view--insert-entries kin)
         (insert "  none\n"))
       (llm-pick-view--insert-header "Cheaper models"
                                     "Bands by output price ratio against this model.")
       (dolist (band cheaper)
         (llm-pick-view--insert-header (car band))
         (llm-pick-view--insert-entries (cdr band)))
       (llm-pick-view--insert-header "Dearer models"
                                     "Bands by output price ratio against this model.")
       (dolist (band dearer)
         (llm-pick-view--insert-header (car band))
         (llm-pick-view--insert-entries (cdr band)))))))

;;; Compare view

(defun llm-pick-view--category-columns (records)
  "Return the categories the capability scores of RECORDS are keyed by."
  (let (categories)
    (dolist (record records)
      (dolist (key (plist-get record :scores))
        (when (consp (car key))
          (cl-pushnew (cdar key) categories :test #'equal))))
    (nreverse categories)))

;;; Side-by-side diff

;;; Dispatching

(defun llm-pick-view-ret ()
  "Open the model view of the model under the cursor."
  (interactive)
  (let ((record (llm-pick-view--entry-at-point)))
    (if record
        (llm-pick-view-model record)
      (user-error "Put the cursor on a model line first"))))

(defun llm-pick-view-quit ()
  "Go back one level: model to main, main to nothing."
  (interactive)
  (pcase llm-pick-view--kind
    ('model (llm-pick-view-main))
    (_ (quit-window))))

(provide 'llm-pick-view)

;;; llm-pick-view.el ends here
