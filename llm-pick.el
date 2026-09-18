;;; llm-pick.el --- Pick the best LLM by capability and price -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: Claude
;; URL: https://github.com/OverbearingPearl/llm-pick
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience

;;; Commentary:

;;
;; llm-pick ranks large language models by capability and price.  It
;; collects scores and prices from several sources, aligns the same
;; model across them, and answers one question: which model is the best
;; one I can afford?
;;
;; Four dimensions describe a query, and they combine:
;;
;;   what to show     :columns :format :mode  see llm-pick-render-report-text and
;;                    :bounds :display        llm-pick--report-body
;;   what to keep     :where :scope :available-on :budget
;;                    :target-score           see llm-pick-query-run-query
;;   where data comes :category :sources :anchor
;;   from                                     see llm-pick-collect
;;   in which order   :order :descending :top see llm-pick-query-run-query
;;
;; A price is always the output price in USD per million tokens, the
;; same axis the Pareto frontier and the price ladder use.
;;
;; Commands:
;;   M-x llm-pick                  the main view of every model
;;   M-x llm-pick-report           a table of the models a query selects
;;   M-x llm-pick-report-query     the same, asking for the query first
;;   M-x llm-pick-report-frontier  the frontier with the gain per step
;;   M-x llm-pick-report-ladder    the models bucketed by price
;;   M-x llm-pick-top-value        the best models by capability per dollar
;;   M-x llm-pick-cheap-strong     capable models that cost little
;;   M-x llm-pick-pick-interactive the best model under a budget
;;   M-x llm-pick-align-report     what the last alignment did
;;   M-x llm-pick-align-check      every ID problem of a run, in one buffer
;;   M-x llm-pick-test-run         the test suite, from llm-pick-test.el
;;
;; A report command answers with its defaults and asks nothing.  With a
;; prefix argument it reads a query on one line instead, and remembers
;; the last one: type the words `category', `budget', `score', `on',
;; `scope', `order', `top', `mode', `columns', `bounds', `where' or
;; `format', then the value.  An empty line means every model, and so
;; does leaving `category' out: without it a model is scored by the best
;; of its categories, which is the choice llm-pick makes for you.
;; `M-x llm-pick-report-query' is the same thing without the prefix.
;; TAB completes the word or the value at point and shows what the line
;; accepts, `M-p' walks the queries of this session, and the prompt
;; itself carries one example of a full line.
;;
;; Programming interface:
;;   (llm-pick-collect :category "coding")
;;   (llm-pick-pick :budget 3.0 :available-on '(openrouter))
;;   (llm-pick-resolve "claude-3-5-sonnet" 'openrouter)
;;   (llm-pick-align '((benchlm . ("claude-3.5-sonnet"))
;;                     (openrouter . ("anthropic/claude-3.5-sonnet")))
;;                   'benchlm)
;;
;; `llm-pick-pick' returns a canonical ID, llm-pick's own key;
;; `llm-pick-resolve' turns that key into the ID an API wants:
;;
;;   (let* ((choice (llm-pick-pick :budget 3.0))
;;          (target (llm-pick-resolve choice 'openrouter)))
;;     (call-the-api (car target) (cdr target)))
;;
;; This module owns the user options, the public API and the interactive
;; commands; the implementation lives in the modules under lisp/,
;; loaded in dependency order: core, align, fetch, source, analyze,
;; query, pick, render.

;;; Code:

(require 'cl-lib)
;; Bootstrap `load-path' from this file's location so that the package
;; works no matter where it is loaded from, including plain `load-file'.
(defconst llm-pick--directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory holding llm-pick.el.")

(defconst llm-pick--lisp-directory
  (expand-file-name "lisp" llm-pick--directory)
  "Directory holding the implementation modules.")

(dolist (dir (list llm-pick--lisp-directory llm-pick--directory))
  (add-to-list 'load-path dir))

;; Submodules, in dependency order: core <- align <- fetch <- source
;; <- analyze <- query <- pick <- render.  Loading this file loads all
;; of them.
(require 'llm-pick-core)
(require 'llm-pick-align)
(require 'llm-pick-fetch-get)
(require 'llm-pick-source)
(require 'llm-pick-analyze)
(require 'llm-pick-query-run)
(require 'llm-pick-query-read)
(require 'llm-pick-pick)
(require 'llm-pick-render-report)
(require 'llm-pick-view)

;;; User options

(defgroup llm-pick nil
  "Pick the best LLM by capability and price."
  :group 'tools
  :prefix "llm-pick-")

(defcustom llm-pick-source-openrouter-api-key nil
  "Bearer token sent with the OpenRouter request.
Nil, the default, falls back to the OPENROUTER_API_KEY environment
variable; when neither is set the header is left out, so a public
endpoint keeps working."
  :type '(choice (const :tag "From the environment" nil) string))

(defcustom llm-pick-core-default-capability-source 'benchlm
  "Source used by the `score' shorthand and the `score' predicate."
  :type 'symbol)

(defcustom llm-pick-core-default-price-source 'benchlm
  "Default price source used by the `in'/`out' price shorthands and predicates.
The other source, `openrouter', is pointed at by
`llm-pick-core-secondary-price-source'."
  :type 'symbol)

(defcustom llm-pick-core-secondary-price-source 'openrouter
  "Second price source the `bm-in', `bm-out' and `gap' fields read.
The `gap' field is the relative difference between this source and
`llm-pick-core-default-price-source', so it says which channel is cheaper.
Defaults to `openrouter', whose OpenRouter prices are read by the `bm-in',
`bm-out' and `gap' fields out of the box; only change this option when
you register a different second price source; see the Sources section
of README.md."
  :type '(choice (const :tag "None" nil) symbol))

(defcustom llm-pick-render-report-marginal-threshold 2.0
  "Marginal gain in points per dollar below which paying more is not advised."
  :type 'number)

(defcustom llm-pick-render-report-default-bar-width 30
  "Default width of the capability bar in the report buffer."
  :type 'integer)

(defcustom llm-pick-report-columns '(name bar score or-out)
  "Columns shown by a report in `table' mode.
Every column is a field `llm-pick-core--field' understands, or `bar' for the
capability bar.  A list of the form (score SOURCE) or
\(price SOURCE DIRECTION\) selects an explicit source."
  :type '(repeat sexp))

(defcustom llm-pick-report-ladder-bounds '(0.5 1 2 5 nil)
  "Upper output price of every bucket of a `ladder' report.
Prices are in USD per million tokens.  The last entry is nil, the open
ended bucket that holds everything above the bound before it."
  :type '(repeat (choice number (const :tag "Open ended" nil))))

(defcustom llm-pick-report-benchmark-bands '(0.5 0.8 1.25 2.0 nil)
  "Output price ratio bands for the benchmark report.

Each number is an output price ratio against the baseline model: 0.5
means half the baseline's output price, and 2.0 means twice as much.
The last entry nil is an open-ended band holding everything above the
band before it.  See `llm-pick-report-benchmark'."
  :type '(repeat (choice number (const nil)))
  :group 'llm-pick)

(defcustom llm-pick-align-match-threshold 0.85
  "Minimum similarity for two IDs to count as the same model."
  :type 'number)

(defcustom llm-pick-align-match-ambiguity-gap 0.05
  "Minimum score gap between the best and the second best match.
A smaller gap makes the match ambiguous and is reported as an error."
  :type 'number)

(defcustom llm-pick-align-match-consensus-sources 2
  "How many sources must spell an ID alike for the agreement to count.
A normalized ID that this many sources arrived at on their own is a model
those sources agree about, and that agreement can settle a match the score
alone leaves just short of `llm-pick-align-match-threshold', see
`llm-pick-align-match-consensus-band'.  Never fewer than two: one source
spelling a name is what a single similarity score already says.

The agreement has to come from a source other than the one being aligned,
so the rule needs three catalogues or more to fire at all; with the two
sources registered by default it never does.  It is meant for a
collection that reads several stores, where a model may be listed by only
some of them."
  :type 'integer)

(defcustom llm-pick-align-match-consensus-band 0.05
  "How far below `llm-pick-align-match-threshold' an agreed match may score.
An ID whose best anchor match scores inside this band, and whose normalized
form at least `llm-pick-align-match-consensus-sources' sources share, is matched
to that anchor model.  A wider band matches more and risks merging two
models that two catalogues happen to name alike; 0 turns the rule off."
  :type 'number)

(defcustom llm-pick-align-on-unmatched 'standalone
  "What to do with an ID that matches no model of the anchor source."
  :type '(choice (const :tag "Keep as a standalone model" standalone)
                 (const :tag "Signal an error" error)))

;;; Programming interface

;;;###autoload
(defun llm-pick-collect (&rest args)
  "Collect model records from the registered sources.
ARGS is a plist with :category, :sources and :anchor, see
`llm-pick-source--collect'.  :category takes one category, or a list of them to
collect side by side.  Return the records ordered by canonical ID."
  (apply #'llm-pick-source--collect args))

;;;###autoload
(defun llm-pick-align (sources &optional anchor)
  "Align model IDs across SOURCES.
SOURCES is an alist of (SOURCE . ID-LIST), ANCHOR names the primary
source and defaults to `llm-pick-core-default-capability-source'.  Return the
alignment report plist, see `llm-pick-align--align'."
  (llm-pick-align--align sources anchor))

;;; Argument checking

(defconst llm-pick--report-key-groups
  '((:category :sources :anchor)
    (:where :scope :available-on :budget :target-score :order :descending :top)
    (:columns :mode :format :bounds :display))
  "Argument groups accepted by `llm-pick-report'.
A key outside these groups is an error instead of a silently ignored
option.")

(defconst llm-pick--pick-key-groups
  '((:category :sources :anchor)
    (:budget :target-score :available-on))
  "Argument groups accepted by `llm-pick-pick'.")

(defun llm-pick--report-args (args group)
  "Return the subset of ARGS whose keys belong to GROUP."
  (cl-loop for (key value) on args by #'cddr
           when (memq key group)
           append (list key value)))

(defun llm-pick--check-args (args groups)
  "Signal `llm-pick-error' when ARGS carries a key outside GROUPS."
  (unless (cl-evenp (length args))
    (signal 'llm-pick-error (list (format "Odd number of arguments: %S" args))))
  (cl-loop for (key _value) on args by #'cddr
           unless (cl-some (lambda (group) (memq key group)) groups)
           do (signal 'llm-pick-error
                      (list (format "Unknown argument: %S" key)))))

(defun llm-pick--report-check-args (args)
  "Signal `llm-pick-error' when ARGS carries a key outside the report groups."
  (llm-pick--check-args args llm-pick--report-key-groups))

;;; Reading a query

(defconst llm-pick--report-query-prompt
  "Report query (RET all, TAB completes; e.g. budget 3 order value): "
  "Prompt the report commands read their query with.")

(defun llm-pick--report-interactive (prefix)
  "Return the argument list of a report command called with PREFIX.
A nil PREFIX asks nothing at all, so the command answers with its
defaults; a prefix argument reads a one line query, see
`llm-pick-query-read-args'."
  (when prefix
    (llm-pick-query-read-args llm-pick--report-query-prompt)))

;;; Reports

(defun llm-pick--report-category-label (category)
  "Return CATEGORY as a report headline names it."
  (cond ((null category) "all categories")
        ((listp category)
         (mapconcat (lambda (item) (format "%s" item)) category " + "))
        (t (format "%s" category))))

(defun llm-pick--report-headline (args count)
  "Return the headline of a report over COUNT models.
ARGS is the plist the report was called with; its category,
:budget and :order all appear in the headline."
  (let ((parts (list (llm-pick--report-category-label
                      (plist-get args :category))
                     (format "%d model%s" count (if (= count 1) "" "s")))))
    (when (plist-get args :budget)
      (setq parts (append parts (list (format "budget $%.2f"
                                              (plist-get args :budget))))))
    (when (plist-get args :order)
      (setq parts (append parts (list (format "by %s%s"
                                              (plist-get args :order)
                                              (if (plist-get args :descending)
                                                  " descending" ""))))))
    (format "=== %s ===" (mapconcat #'identity parts " | "))))

(defun llm-pick--report-body (args models)
  "Return the rendered body of the report that ARGS asks for over MODELS."
  (pcase (or (plist-get args :mode) 'table)
    ('table (llm-pick-render-report-text models
                             (or (plist-get args :columns)
                                 llm-pick-report-columns)
                             (or (plist-get args :format) 'table)))
    ('frontier (llm-pick-render-report-frontier models))
    ('ladder (llm-pick-render-report-ladder
              models
              (or (plist-get args :bounds) llm-pick-report-ladder-bounds)))
    (_ (signal 'llm-pick-error
               (list (format "Unknown report mode: %S, expected one of %S"
                             (plist-get args :mode)
                             '(table frontier ladder)))))))

(defun llm-pick--report-text (args models)
  "Return the text of the report that ARGS asks for over MODELS."
  (concat (llm-pick--report-headline args (length models)) "\n\n"
          (llm-pick--report-body args models) "\n"))

(define-derived-mode llm-pick-report-mode special-mode "llm-pick"
  "Major mode of the `llm-pick-report' buffer."
  (setq-local truncate-lines nil))

(defun llm-pick--report-display (text &optional buffer-name)
  "Show TEXT in the BUFFER-NAME, `*llm-pick*' by default, and return TEXT."
  (let ((buffer (get-buffer-create (or buffer-name "*llm-pick*"))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text)
        (goto-char (point-min))
        (llm-pick-report-mode)))
    (pop-to-buffer buffer)
    text))

;;;###autoload
(defun llm-pick-report (&rest args)
  "Show the models the arguments ask for in the `*llm-pick*' buffer.
Return the text of the report.

ARGS is a plist of three groups:

  collection  :category :sources :anchor, see `llm-pick-collect'
  selection   :where :scope :available-on (:providers) :budget
              :target-score :order :descending :top, see
              `llm-pick-query-run-query'
  rendering   :mode :columns :format :bounds :display

:category takes one category or a list of them; a record collected for
several keys its scores by (SOURCE . CATEGORY), which is the
`(score SOURCE CATEGORY)' column.

:mode is `table' (the default), `frontier' for the Pareto frontier with
the gain of every step up, or `ladder' for the models bucketed by output
price.  A `table' report takes :columns and :format, a `ladder' report
takes :bounds.  A budget is an output price in USD per million tokens,
the axis the analysis uses.  Pass :display nil to get the text without
showing the buffer.  An unknown argument is an error, not a silently
ignored option.

Interactively, report every model and ask nothing.  A prefix argument
reads a one line query first, the same one `M-x llm-pick-report-query'
reads without the prefix; see `llm-pick-query-read-args' for the words the
line takes and `llm-pick-query-read-last-query' for the default it offers."
  (interactive (llm-pick--report-interactive current-prefix-arg))
  (setq args (llm-pick-query-run--expand-aliases args))
  (llm-pick--report-check-args args)
  (let* ((models (apply #'llm-pick-collect
                        (llm-pick--report-args
                         args '(:category :sources :anchor))))
         (selected (apply #'llm-pick-query-run-query
                         models
                         (llm-pick--report-args
                          args '(:where :scope :available-on :budget
                                :target-score :order :descending :top))))
         (text (llm-pick--report-text args selected)))
    ;; A present :display nil means "give me the text", an absent one
    ;; means "show it".
    (if (and (memq :display args) (null (plist-get args :display)))
        text
      (llm-pick--report-display text))))

;;;###autoload
(defun llm-pick-report-frontier (&rest args)
  "Show the Pareto frontier and the gain of every step up in price.
A prefix argument reads a one line query first, see
`llm-pick-query-read-args'; the view is fixed, so a `mode' word in the query
has no effect.

ARGS is a plist for `llm-pick-report'."
  (interactive (llm-pick--report-interactive current-prefix-arg))
  (apply #'llm-pick-report :mode 'frontier args))

;;;###autoload
(defun llm-pick-report-ladder (&rest args)
  "Show the models bucketed by output price.
A prefix argument reads a one line query first, see
`llm-pick-query-read-args'; the view is fixed, so a `mode' word in the query
has no effect.

ARGS is a plist for `llm-pick-report'."
  (interactive (llm-pick--report-interactive current-prefix-arg))
  (apply #'llm-pick-report :mode 'ladder args))

;;;###autoload
(defun llm-pick-top-value ()
  "Show the ten models with the most capability per dollar.
A `table' report over every model, ordered by the `value' field, the
capability score divided by the output price, so the models that give
the most capability for the money come first."
  (interactive)
  (llm-pick-report :order 'value :descending t :top 10))

;;;###autoload
(defun llm-pick-report-query ()
  "Ask for a one line query and show the report of what it selected.
This is what a report command becomes with a prefix argument; it exists
so that the menu can offer the query without one."
  (interactive)
  (apply #'llm-pick-report
         (llm-pick-query-read-args llm-pick--report-query-prompt)))

;;;###autoload
(defun llm-pick-cheap-strong ()
  "Show the capable models that cost little.
Every model scoring at least 75 for less than $5 per million output
tokens, strongest first."
  (interactive)
  (llm-pick-report :target-score 75 :budget 5.0 :order 'score :descending t))

;;;###autoload
(defun llm-pick-report-benchmark (baseline &rest args)
  "Show the models around BASELINE, bucketed by output price ratio.
BASELINE is matched loosely: its case, its hyphens and its spaces do
not matter, and the canonical ID, the display name and the provider
IDs are all searched, so `DeepSeek 3', `deepseek-v3' and
`deepseek-v3.2' all find the model.

The bands are output price ratios around the baseline's own output
price, not absolute prices, so the report stays useful when the
baseline moves.  ARGS is a plist for `llm-pick-collect', such as
:category, :sources and :anchor; the bands come from
`llm-pick-report-benchmark-bands'."
  (interactive (list (read-string "Baseline model (e.g. deepseek-v3): ")))
  (let* ((records (apply #'llm-pick-collect args))
         (matches (llm-pick-core--filter
                   records (list (list '~ 'name baseline))))
         (baseline-record (or (cl-find baseline matches
                                       :key (lambda (record)
                                              (llm-pick-core--field record 'name))
                                       :test #'equal)
                              (car matches))))
    (unless baseline-record
      (signal 'llm-pick-error
              (list (format "No model matches %S" baseline))))
    (let ((text (llm-pick-render-report-benchmark
                 records baseline-record
                 llm-pick-report-benchmark-bands)))
      (llm-pick--report-display text "*llm-pick*")
      text)))

(defun llm-pick-align-report-text (&optional all)
  "Return the text of the report of the last alignment.
ALL is passed on to `llm-pick-render-report-alignment'."
  (let ((report llm-pick-align--last-report))
    (unless report
      (signal 'llm-pick-error
              (list "No alignment has run yet; call `llm-pick-collect' first")))
    (llm-pick-render-report-alignment report all)))

;;;###autoload
(defun llm-pick-align-report (&optional all)
  "Show what the last call to `llm-pick-align' did.

The report names every decision a run made: which ID came from which
source, what the normalizer turned it into and the canonical model it was
taken for, grouped by how the two were brought together.  ALL, the prefix
argument interactively, also lists the IDs that normalize to the anchor's
model already, which is where a normalization rule that drops too much
would show."
  (interactive "P")
  (llm-pick--report-display (llm-pick-align-report-text all)
                            "*llm-pick-align*"))

;;;###autoload
(defun llm-pick-align-check (&rest args)
  "Show every ID problem a collection meets, not only the first.
Return the text it shows.  ARGS is a plist for `llm-pick-collect'; with
no argument at all it checks every model of every registered source,
which is the widest net, and it costs one collection.

An alignment normally stops at the first problem, because a report built
on a wrong canonical ID is worse than no report.  This command walks the
whole catalogue instead and renders the problems grouped by kind, so
that a source which trips three normalization rules can be fixed in one
pass.  An ID problem is shown, never signaled; a source that cannot be
read at all still signals `llm-pick-error'."
  (interactive)
  (let ((llm-pick-align--collecting t)
        (llm-pick-align--problems nil))
    (apply #'llm-pick-collect args)
    (llm-pick--report-display
     (llm-pick-render-report-problems llm-pick-align--problems))))

;;; Choosing

(defun llm-pick--pick-record (args)
  "Return the record `llm-pick-pick' would answer ARGS with.
ARGS is written as for `llm-pick-pick'; `:providers' is a spelling of
`:available-on' here too."
  (setq args (llm-pick-query-run--expand-aliases args))
  (llm-pick--check-args args llm-pick--pick-key-groups)
  (let* ((models (apply #'llm-pick-collect
                        (llm-pick--report-args
                         args '(:category :sources :anchor))))
         (choice (apply #'llm-pick-pick--choose
                        models
                        (llm-pick--report-args
                         args '(:budget :target-score :available-on)))))
    (llm-pick-pick--find models (llm-pick-core--field choice 'name))))

(defun llm-pick-pick (&rest args)
  "Return the canonical ID of the best model ARGS allow.

ARGS is a plist of two groups:

  collection  :category :sources :anchor, see `llm-pick-collect'
  selection   :budget :target-score :available-on (:providers), see
              `llm-pick-pick--choose'

The most capable candidate wins and the cheaper one wins a tie.  The
result is guaranteed to carry an ID for a provider in `:available-on',
so `llm-pick-resolve' can always turn it into a callable ID.  Signal
`llm-pick-error' when no model satisfies the criteria."
  (llm-pick-core--field (llm-pick--pick-record args) 'name))

;;;###autoload
(defun llm-pick-resolve (canonical provider &rest collection)
  "Return the (PROVIDER . ID) pair that can call CANONICAL.
COLLECTION is a plist for `llm-pick-collect', written the same way it is
written for `llm-pick-pick'.  Signal `llm-pick-error' when no model
carries CANONICAL, or when it has no ID for PROVIDER."
  (llm-pick-pick--resolve (apply #'llm-pick-collect collection)
                     canonical provider))

;;;###autoload
(defun llm-pick-resolve-with-fallback (canonical providers &rest collection)
  "Return the first (PROVIDER . ID) pair of CANONICAL among PROVIDERS.
PROVIDERS is tried in order.  COLLECTION is a plist for
`llm-pick-collect', written the same way as for `llm-pick-pick'."
  (llm-pick-pick--resolve-with-fallback (apply #'llm-pick-collect collection)
                                   canonical providers))

;;;###autoload
(defun llm-pick-pick-interactive (&rest args)
  "Ask for a query and echo the best model that satisfies it.
The query is one line; for a choice the useful words are `category',
`budget', `score' and `on', see `llm-pick-query-read-args'.  An empty line
picks the most capable model of all, with no budget and no target
score.
ARGS is a plist for `llm-pick-pick'."
  (interactive (llm-pick-query-read-args
                "Pick query (RET most capable, TAB completes; e.g. budget 3 on openrouter): "
                nil llm-pick-query-read-pick-words))
  (let ((record (llm-pick--pick-record args)))
    (message "%s scores %s and costs $%s/M out"
             (llm-pick-core--field record 'name)
             (or (llm-pick-core--field record 'score) "n/a")
             (or (llm-pick-core--field record 'or-out) "n/a"))))

;;;###autoload
(defun llm-pick ()
  "Show the main view of every model from every registered source.
The data is fetched once from every source and cached for
`llm-pick-view-cache-ttl' seconds, so repeated calls do not download
again.  The buffer supports hjkl/npfb cursor motion, RET opens the
model view of the entry at point and q quits; see the header line."
  (interactive)
  (llm-pick-view-main))

(provide 'llm-pick)

;;; llm-pick.el ends here
