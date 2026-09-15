;;; llm-pick-query-read.el --- Read a one line query in the minibuffer -*- lexical-binding: t; -*-

;; Copyright (C) 2025 madachuan
;; Author: madachuan <madachuan.noreply.github.com>
;; Assisted-by: Claude
;; URL: https://github.com/madachuan/llm-pick
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The one line query language of the report commands: the words the
;; line takes, the reader that turns a line into an argument plist and
;; the completion that shows what the line accepts.  The commands that
;; ask for a query live in llm-pick.el.

;;; Code:

(require 'cl-lib)
(require 'llm-pick-core)
(require 'llm-pick-source)

(defconst llm-pick-query-read-words
  '(("category" :category "capability category, comma separated for several")
    ("sources" :sources "names of the sources to collect from")
    ("anchor" :anchor "source whose IDs define the canonical IDs")
    ("where" :where "predicates, for instance score>80,or-out<5")
    ("scope" :scope "both, capability-only or price-only")
    ("on" :available-on "providers a model must be callable on")
    ("providers" :available-on "same as on")
    ("budget" :budget "highest output price in USD per million tokens")
    ("score" :target-score "lowest capability score")
    ("order" :order "field to sort by, comma separated for a tie break")
    ("descending" :descending "reverse the order: yes or no")
    ("top" :top "keep at most this many models")
    ("mode" :mode "table, frontier or ladder")
    ("columns" :columns "column names, comma separated")
    ("format" :format "table or csv")
    ("bounds" :bounds "price buckets, for instance 1,2,5,open"))
  "Words the one line query of `llm-pick-query-read-args' understands.
Each entry is (WORD KEY HELP): WORD is what the user types, KEY is the
argument it sets and HELP is the note the completion shows next to it.")

(defvar llm-pick-query-read-last-query nil
  "The query line the next `llm-pick-query-read-args' offers as its default.")

(defvar llm-pick-query-read-query-history nil
  "History of the query lines the report commands read.
`M-p' in the query minibuffer walks it, so a line that only changes one
value can be recalled instead of retyped.")

(defconst llm-pick-query-read-fields
  '("name" "provider" "scope" "score" "or-in" "or-out"
    "bm-in" "bm-out" "gap" "value")
  "Fields a query can order by or show as a column.
It has to stay in step with the fields `llm-pick-core--field' accepts: a name
here that it does not know would offer a column the report then refuses
to render.")

(defun llm-pick-query-read-key (word)
  "Return the argument key of the query WORD, or nil."
  (nth 1 (assoc word llm-pick-query-read-words)))

(defun llm-pick-query-read-help (word)
  "Return the note the query WORD carries, or nil."
  (nth 2 (assoc word llm-pick-query-read-words)))

(defun llm-pick-query-read-values (key)
  "Return the values the query word for KEY suggests, or nil.
The list is a hint for completion, never a restriction: a value it does
not hold is read all the same, so a category a source carries still
works when `llm-pick-category-suggestions' does not list it."
  (pcase key
    ;; A category is llm-pick's own choice, so no list of them is
    ;; offered: leaving the word out scores every model by the best of
    ;; its categories, which is what almost every question wants.
    (:sources (llm-pick-source--names))
    (:anchor (llm-pick-source--names))
    (:where '("score>80" "or-out<5" "value>10" "provider=openrouter"
              "name~claude"))
    (:scope '("both" "capability-only" "price-only"))
    (:available-on (llm-pick-source--names))
    (:budget '("1" "3" "10"))
    (:target-score '("75" "80" "90"))
    (:order llm-pick-query-read-fields)
    (:descending '("yes" "no"))
    (:top '("5" "10" "20"))
    (:mode '("table" "frontier" "ladder"))
    (:columns (cons "bar" llm-pick-query-read-fields))
    (:format '("table" "csv"))
    (:bounds '("0.5,1,2,5,open" "1,2,5,open"))
    (_ nil)))

(defun llm-pick-query-read-candidates (start)
  "Return what the query line accepts at the token starting at START.
START is a position in the minibuffer of a query line.  An even number of
tokens before START means the line waits for a word, an odd one for the
value of the word right before it."
  (let ((before (split-string (buffer-substring-no-properties
                               (minibuffer-prompt-end) start)
                              "[ \t]+" t)))
    (if (cl-evenp (length before))
        (mapcar #'car llm-pick-query-read-words)
      (llm-pick-query-read-values
       (llm-pick-query-read-key (car (last before)))))))

(defun llm-pick-query-read-annotate (candidate)
  "Return the note shown next to CANDIDATE in the completion buffer."
  (let ((help (llm-pick-query-read-help candidate)))
    (when help (concat "  " help))))

(defun llm-pick-query-read-complete ()
  "Complete the query word or value at point.
Bound to TAB while a query line is read, so that the user sees what the
line accepts instead of having to remember it.  The candidates are
suggestions: a value they do not hold is read all the same."
  (interactive)
  (let* ((end (point))
         (start (save-excursion (skip-chars-backward "^ \t") (point)))
         (candidates (llm-pick-query-read-candidates start)))
    (unless candidates
      (user-error "Nothing to complete here; see `llm-pick-query-read-args'"))
    (let* ((word (buffer-substring-no-properties start end))
           (completion-extra-properties
            '(:annotation-function llm-pick-query-read-annotate))
           (choice (completing-read "Complete: " candidates nil nil word)))
      (delete-region start end)
      (insert (if (consp choice) (car choice) choice)))))

(defvar llm-pick-query-read-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map "\t" #'llm-pick-query-read-complete)
    map)
  "Keymap of the minibuffer while a query line is read.")

(defun llm-pick-query-read-predicate (text)
  "Return TEXT, written as `score>80' or `name~claude', as a predicate."
  (if (string-match "\\`\\([^<>=~]+\\)\\(>=\\|<=\\|[<>~=]\\)\\(.*\\)\\'" text)
      (let* ((field (intern (match-string 1 text)))
             (operator (match-string 2 text))
             (raw (match-string 3 text))
             (value (if (memq (aref operator 0) '(?~ ?=))
                        raw
                      (string-to-number raw))))
        (list (intern operator) field value))
    (signal 'llm-pick-error
            (list (format "Cannot read the predicate %S; write it as score>80"
                          text)))))

(defun llm-pick-query-read-value (key text)
  "Return TEXT, the value the query word for KEY carries."
  (pcase key
    ((or :budget :target-score :top)
     (if (string-match-p "\\`[0-9.]+\\'" text)
         (string-to-number text)
       (signal 'llm-pick-error
               (list (format "%S wants a number, got %S" key text)))))
    (:descending (not (member text '("nil" "false" "no"))))
    (:category text)
    ((or :scope :mode :format :order :anchor) (intern text))
    ((or :sources :available-on :columns)
     (mapcar #'intern (split-string text "," t)))
    (:bounds
     (mapcar (lambda (word)
               (if (member word '("open" "nil" "inf"))
                   nil
                 (string-to-number word)))
             (split-string text "," t)))
    (:where (mapcar #'llm-pick-query-read-predicate (split-string text "," t)))
    (_ (signal 'llm-pick-error
               (list (format "No reader for the query word %S" key))))))

(defun llm-pick-query-read-args (prompt &optional initial)
  "Read a report query from the minibuffer and return it as a plist.
PROMPT names what the query is for.  INITIAL, when non-nil, is the
line the minibuffer starts with; nil falls back to
`llm-pick-query-read-last-query'.  The query is the words of
`llm-pick-query-read-words', each followed by its value, separated by
spaces: \"category coding budget 3 order value top 5\".  Return nil for
an empty line, which asks for every model.

TAB completes the word or the value at point, showing what the line
accepts, and `M-p' walks the queries of this session, so the prompt only
has to carry one example of a full line."
  (let* ((line (minibuffer-with-setup-hook
                   (lambda () (use-local-map llm-pick-query-read-minibuffer-map))
                 (read-string prompt (or initial llm-pick-query-read-last-query)
                              'llm-pick-query-read-query-history)))
         (words (split-string line "[ \t]+" t))
         (args nil))
    (setq llm-pick-query-read-last-query line)
    (while words
      (let* ((word (pop words))
             (key (llm-pick-query-read-key word)))
        (unless key
          (signal 'llm-pick-error
                  (list (format "Unknown query word %S; press TAB to see the %d words there are, or RET for every model"
                                word (length llm-pick-query-read-words)))))
        (let ((text (pop words)))
          (unless text
            (signal 'llm-pick-error
                    (list (format "The query word %S needs a value, for instance %s"
                                  word
                                  (or (car (llm-pick-query-read-values key)) "one")))))
          (setq args (append args
                             (list key (llm-pick-query-read-value key text)))))))
    args))

(provide 'llm-pick-query-read)

;;; llm-pick-query-read.el ends here
