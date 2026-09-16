;;; llm-pick-view.el --- Interactive main, model and compare views -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: madachuan <madachuan.noreply.github.com>
;; Assisted-by: DeepSeek:deepseek-v4-flash, GLM:glm-5.3-flash, Laguna:laguna-s-2.1
;; URL: https://github.com/madachuan/llm-pick
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
  "Interactive views of llm-pick."
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

(defcustom llm-pick-view-cache-ttl 86400
  "Seconds a collected set of records stays fresh on disk.
86400 is one day: within it, opening a view downloads nothing."
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

(defvar-local llm-pick-view--filter nil
  "Predicate a main view keeps its models with, or nil.")

(defvar-local llm-pick-view--order nil
  "Field a main view sorts its models by, or nil for the canonical ID.")

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
         (setq llm-pick-view--records (llm-pick-source--collect))
         (llm-pick-view--save-cache llm-pick-view--records)
         llm-pick-view--records)))

;;; Record helpers

(defun llm-pick-view--record-sources (record)
  "Return the sorted names of the sources that name RECORD."
  (let (sources)
    (dolist (key (plist-get record :scores))
      (cl-pushnew (if (consp (car key)) (caar key) (car key)) sources))
    (dolist (pair (plist-get record :prices))
      (cl-pushnew (car pair) sources))
    (sort sources (lambda (a b) (string< (symbol-name a) (symbol-name b))))))

(defun llm-pick-view--num (value)
  "Return VALUE as a short cell, or a dash."
  (if (numberp value) (format "%g" value) "-"))

(defun llm-pick-view--line (record)
  "Return one summary line for RECORD.
Columns: model name, overall score, BenchLM category scores,
Artificial Analysis scores, value, prices, and OpenRouter id."
  (let* ((benchlm-categories
          (sort (cl-loop for (key . _) in (plist-get record :scores)
                         when (and (consp key) (eq (car key) 'benchlm))
                         collect (cdr key))
                #'string<))
         (aa-categories '("intelligence" "coding" "agentic"))
         (cells (append
                 (list (format "%-44s"
                               (or (plist-get record :display-name)
                                   (llm-pick-core--field record 'name))))
                 (list (llm-pick-view--num (llm-pick-core--field record 'score)))
                 (mapcar
                  (lambda (cat)
                    (llm-pick-view--num
                     (llm-pick-core--score record 'benchlm cat)))
                  benchlm-categories)
                 (mapcar
                  (lambda (cat)
                    (llm-pick-view--num
                     (llm-pick-core--score record 'artificial-analysis cat)))
                  aa-categories)
                 (list (llm-pick-view--num (llm-pick-core--field record 'value)))
                 (list (llm-pick-view--num (llm-pick-core--field record 'bm-in))
                       (llm-pick-view--num (llm-pick-core--field record 'bm-out)))
                 (list (llm-pick-view--num (llm-pick-core--field record 'or-in))
                       (llm-pick-view--num (llm-pick-core--field record 'or-out)))
                 (list (or (cdr (assq 'openrouter (plist-get record :providers)))
                           "-")))))
    (mapconcat #'identity cells "  ")))

;;; Rendering primitives

(defun llm-pick-view--insert-header (title &optional note)
  "Insert a section header TITLE with an optional NOTE under it."
  (let ((start (point)))
    (insert (propertize title 'face 'llm-pick-view-header-face) "\n")
    (when note (insert note "\n"))
    (put-text-property start (point) 'llm-pick-header t)))

(defun llm-pick-view--insert-columns (&optional _columns)
  "Insert the column header line with `llm-pick-view-column-face'."
  (let* ((benchlm-cats '("Agentic" "Coding" "Reasoning" "Multimodal"
                         "Knowledge" "Multiling" "Instr-F" "Math"))
         (aa-cats '("AA-Int" "AA-Code" "AA-Agnt"))
         (head (concat (format "%-44s  %9s" "Model" "Score")))
         (head (concat head "  " (mapconcat (lambda (c) (format "%9s" c)) benchlm-cats "  ")))
         (head (concat head "  " (mapconcat (lambda (c) (format "%9s" c)) aa-cats "  ")))
         (head (concat head "  " (format "%9s" "Value")))
         (head (concat head "  " (format "%14s" "In/Out (BL)")))
         (head (concat head "  " (format "%14s" "In/Out (OR)")))
         (head (concat head "  " (format "%24s" "OpenRouter id")))
         (start (point)))
    (insert (propertize head 'face 'llm-pick-view-column-face) "\n")
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
          header-line-format (llm-pick-view--header-line kind))
    (setq buffer-read-only t)))

(defun llm-pick-view--header-line (kind)
  "Return the key hint for the header line of a view of KIND."
  (concat "llm-pick "
          (pcase kind
            ('main
             "[n/p j/k entry] [f/b section] [RET model] [s price | S score | v value] [F filter] [g refresh] [q quit]")
            ('model
             "[n/p j/k entry] [f/b section] [RET model] [c compare] [q back]")
            ('compare
             "[n/p j/k entry] [f/b section] [RET model] [d side-by-side diff] [q model view of point]"))))

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
    (define-key map "s" 'llm-pick-view-sort-price)
    (define-key map "S" 'llm-pick-view-sort-score)
    (define-key map "v" 'llm-pick-view-sort-value)
    (define-key map "F" 'llm-pick-view-filter)
    (define-key map "c" 'llm-pick-view-compare-here)
    (define-key map "d" 'llm-pick-view-diff-here)
    map)
  "Keymap of `llm-pick-view-mode'.")

(defvar llm-pick-view--jump-target nil
  "Record the view returns to with `q', when it is not the parent.")

(define-derived-mode llm-pick-view-mode special-mode "llm-pick-view"
  "Major mode of the llm-pick main, model and compare views."
  (setq-local truncate-lines t))

;;; Main view

(defun llm-pick-view--sort (records)
  "Return RECORDS in the order of the main view."
  (if (null llm-pick-view--order)
      (sort (copy-sequence records)
            (lambda (a b)
              (string< (llm-pick-core--field a 'name)
                       (llm-pick-core--field b 'name))))
    (sort (copy-sequence records)
          (lambda (a b)
            (> (or (llm-pick-core--field a llm-pick-view--order) -1)
               (or (llm-pick-core--field b llm-pick-view--order) -1))))))

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
        (if (> (length sources) 1)
            (progn
              (setq shared-key (mapcar #'symbol-name sources))
              (puthash shared-key (cons record (gethash shared-key shared))
                       shared))
          (let ((key (list (mapcar #'symbol-name sources))))
            (puthash key (cons record (gethash key only)) only)))))
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
         (kept (if llm-pick-view--filter
                   (cl-remove-if-not llm-pick-view--filter records)
                 records))
         (groups (llm-pick-view--main-groups (llm-pick-view--sort kept))))
    (pop-to-buffer (get-buffer-create "*llm-pick*"))
    (llm-pick-view-mode)
    (llm-pick-view--render
     'main
     (format "llm-pick: %d of %d models"
             (length kept) (length records))
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

(defun llm-pick-view-sort-price ()
  "Sort the main view by output price, cheapest first."
  (interactive)
  (setq llm-pick-view--order 'or-out)
  (let ((llm-pick-view--filter llm-pick-view--filter))
    (llm-pick-view-main--sorted
     (lambda (a b)
       (< (or (llm-pick-core--field a 'or-out) most-positive-fixnum)
          (or (llm-pick-core--field b 'or-out) most-positive-fixnum))))))

(defun llm-pick-view-sort-score ()
  "Sort the main view by capability score, strongest first."
  (interactive)
  (setq llm-pick-view--order 'score)
  (llm-pick-view-main))

(defun llm-pick-view-sort-value ()
  "Sort the main view by capability per dollar, best first."
  (interactive)
  (setq llm-pick-view--order 'value)
  (llm-pick-view-main))

(defun llm-pick-view-main--sorted (less-p)
  "Re-render the main view with the internal LESS-P order."
  (let* ((records (llm-pick-view--models))
         (kept (if llm-pick-view--filter
                   (cl-remove-if-not llm-pick-view--filter records)
                 records))
         (groups (llm-pick-view--main-groups
                  (sort (copy-sequence kept) less-p))))
    (with-current-buffer "*llm-pick*"
      (llm-pick-view--render
       'main (format "llm-pick: %d of %d models" (length kept) (length records))
       (lambda ()
         (dolist (group groups)
           (llm-pick-view--insert-header (car group))
           (llm-pick-view--insert-entries (cdr group))))))))

(defun llm-pick-view-filter ()
  "Ask for a price cap and per-category score floors, keep what fits.
An empty answer at any prompt means no limit on that axis."
  (interactive)
  (let* ((cap-text (read-string "Max output price $/M (empty = all): "))
         (cap (and (string-match-p "\\`[0-9.]+\\'" cap-text)
                   (string-to-number cap-text)))
         (source (symbol-value 'llm-pick-core-default-capability-source))
         (floors nil))
    (dolist (category (llm-pick-query-read-values :category))
      (let* ((answer (read-string (format "Min %s score (empty = all): "
                                          category)))
             (floor (and (string-match-p "\\`[0-9.]+\\'" answer)
                         (string-to-number answer))))
        (when floor
          (push (cons category floor) floors))))
    (setq llm-pick-view--filter
          (lambda (record)
            (and (or (null cap)
                     (and (numberp (llm-pick-core--field record 'or-out))
                          (<= (llm-pick-core--field record 'or-out) cap)))
                 (cl-every
                  (lambda (entry)
                    (let ((score (llm-pick-core--score record source
                                                       (car entry))))
                      (and (numberp score) (>= score (cdr entry)))))
                  floors))))
    (llm-pick-view-main)))

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
          (format "Scope:     %s" (llm-pick-core--field record 'scope))
          (format "Value:     %s"
                  (llm-pick-view--num (llm-pick-core--field record 'value))))))
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

(defun llm-pick-view-compare (baseline)
  "Show the compare view of BASELINE: every category, stronger and weaker.
The model view model stays at the top so the two can be read side by
side; several compare views of one model view can be open at once."
  (let* ((records (llm-pick-view--models))
         (others (cl-remove-if (lambda (other) (equal other baseline)) records))
         (source (symbol-value 'llm-pick-core-default-capability-source))
         (categories (or (llm-pick-view--category-columns records) '(nil)))
         (buffer (get-buffer-create
                  (format "*llm-pick compare %s*"
                          (llm-pick-core--field baseline 'name)))))
    (pop-to-buffer buffer)
    (llm-pick-view-mode)
    (setq llm-pick-view--baseline baseline)
    (llm-pick-view--render
     'compare (format "llm-pick compare: %s"
                      (llm-pick-core--field baseline 'name))
     (lambda ()
       (insert (llm-pick-view--metadata baseline) "\n")
       (llm-pick-view--insert-header "Baseline" "This is the model the sections below compare against.")
       (insert "")
       (dolist (category categories)
         (let (stronger weaker)
           (dolist (other others)
             (let ((delta (llm-pick-analyze--benchmark-delta
                           other baseline source category)))
               (when delta
                 (if (> delta 0) (push other stronger) (push other weaker)))))
           (llm-pick-view--insert-header
            (if category
                (format "%s: stronger than the baseline" category)
              "Stronger than the baseline"))
           (if stronger (llm-pick-view--insert-entries stronger)
             (insert "  none\n"))
           (llm-pick-view--insert-header
            (if category
                (format "%s: weaker than the baseline" category)
              "Weaker than the baseline"))
           (if weaker (llm-pick-view--insert-entries weaker)
             (insert "  none\n"))))))))

(defun llm-pick-view-compare-here ()
  "Open the compare view around the model view's model."
  (interactive)
  (if llm-pick-view--baseline
      (llm-pick-view-compare llm-pick-view--baseline)
    (user-error "No model to compare against here")))

;;; Side-by-side diff

(defun llm-pick-view-diff (model baseline)
  "Show MODEL against BASELINE, every field side by side, in one buffer."
  (let* ((source (symbol-value 'llm-pick-core-default-capability-source))
         (categories (delete-dups
                      (append (llm-pick-view--category-columns (list model))
                              (llm-pick-view--category-columns (list baseline)))))
         (rows
          (append
           (list
            (list "Vendor"
                  (or (llm-pick-core--field baseline 'vendor) "-")
                  (or (llm-pick-core--field model 'vendor) "-"))
            (list "Family"
                  (or (llm-pick-core--field baseline 'family) "-")
                  (or (llm-pick-core--field model 'family) "-"))
            (list "Score"
                  (llm-pick-view--num (llm-pick-core--field baseline 'score))
                  (llm-pick-view--num (llm-pick-core--field model 'score)))
            (list "$/M out"
                  (llm-pick-view--num (llm-pick-core--field baseline 'or-out))
                  (llm-pick-view--num (llm-pick-core--field model 'or-out)))
            (list "$/M in"
                  (llm-pick-view--num (llm-pick-core--field baseline 'or-in))
                  (llm-pick-view--num (llm-pick-core--field model 'or-in)))
            (list "Score/$"
                  (llm-pick-view--num (llm-pick-core--field baseline 'value))
                  (llm-pick-view--num (llm-pick-core--field model 'value))))
           (mapcar (lambda (category)
                     (list (format "Score %s" (or category "default"))
                           (llm-pick-view--num
                            (llm-pick-core--score baseline source category))
                           (llm-pick-view--num
                            (llm-pick-core--score model source category))))
                   categories)
           (list
            (list "Providers"
                  (mapconcat (lambda (p) (format "%s" (car p)))
                             (plist-get baseline :providers) ", ")
                  (mapconcat (lambda (p) (format "%s" (car p)))
                             (plist-get model :providers) ", ")))))
         (width (apply #'max 10 (mapcar (lambda (row) (length (car row))) rows)))
         (width-a (apply #'max 20
                         (mapcar (lambda (row) (length (nth 1 row))) rows)))
         (buffer (get-buffer-create
                  (format "*llm-pick %s vs %s*"
                          (llm-pick-core--field baseline 'name)
                          (llm-pick-core--field model 'name)))))
    (pop-to-buffer buffer)
    (llm-pick-view-mode)
    (setq llm-pick-view--baseline baseline
          llm-pick-view--kind 'compare)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "%s against %s\n\n"
                      (llm-pick-core--field baseline 'name)
                      (llm-pick-core--field model 'name)))
      (insert (concat (llm-pick-render-report--pad "Field" width nil) "  "
                      (llm-pick-render-report--pad
                       (llm-pick-core--field baseline 'name) width-a nil) "  "
                      (llm-pick-core--field model 'name) "\n"))
      (dolist (row rows)
        (insert (concat (llm-pick-render-report--pad (car row) width nil) "  "
                        (llm-pick-render-report--pad (nth 1 row) width-a nil) "  "
                        (nth 2 row) "\n")))
      (goto-char (point-min))
      (setq header-line-format
            (llm-pick-view--header-line 'compare)))))

(defun llm-pick-view-diff-here ()
  "Show the model under the cursor against the baseline, side by side."
  (interactive)
  (let ((model (llm-pick-view--entry-at-point)))
    (if (and model llm-pick-view--baseline)
        (llm-pick-view-diff model llm-pick-view--baseline)
      (user-error "Put the cursor on a model line first"))))

;;; Dispatching

(defun llm-pick-view-ret ()
  "Open the model view of the model under the cursor."
  (interactive)
  (let ((record (llm-pick-view--entry-at-point)))
    (if record
        (llm-pick-view-model record)
      (user-error "Put the cursor on a model line first"))))

(defun llm-pick-view-quit ()
  "Go back one level: compare to model, model to main, main to nothing."
  (interactive)
  (pcase llm-pick-view--kind
    ('compare
     (let ((record (or (llm-pick-view--entry-at-point)
                       llm-pick-view--baseline)))
       (if record (llm-pick-view-model record) (llm-pick-view-main))))
    ('model (llm-pick-view-main))
    (_ (quit-window))))

(provide 'llm-pick-view)

;;; llm-pick-view.el ends here
