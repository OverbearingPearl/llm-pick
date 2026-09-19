;;; llm-pick-render-report.el --- Render model records as text -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: Claude
;; URL: https://github.com/OverbearingPearl/llm-pick
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;
;; Turning records into text is a pure operation: every function here
;; takes records and returns a string, so the report commands stay thin
;; and the layout is testable without a buffer.
;;
;; A column is a field name that `llm-pick-core--field' understands, or the
;; symbol `bar', which draws the capability bar.  The column list is all
;; the layout configuration there is:
;;
;;   (name bar score or-out)          capability and price
;;   (name (score openrouter))        an explicit capability source
;;   (name score (price anthropic out))
;;
;; `llm-pick-render-report-text' is the entry point; it dispatches on the format so
;; that a caller can ask for a table or for CSV.

;;; Code:

(require 'cl-lib)
(require 'llm-pick-core)
(require 'llm-pick-analyze)
(require 'llm-pick-align)

;; The user options live in llm-pick.el, which requires this module.
(defvar llm-pick-render-report-default-bar-width)
(defvar llm-pick-render-report-marginal-threshold)

(defconst llm-pick-render-report--missing "—"
  "Text standing for a value the model does not carry.")

(defconst llm-pick-render-report--headers
  '((name . "Model")
    (provider . "Provider")
    (vendor . "Vendor")
    (family . "Family")
    (scope . "Scope")
    (score . "Score")
    (bar . "Capability")
    (or-in . "$/M in")
    (or-out . "$/M out")
    (bm-in . "2nd $/M in")
    (bm-out . "2nd $/M out")
    (gap . "Gap")
    (gain . "Gain/$")
    (bound . "Up to $/M out"))
  "Header text of the columns known by name.")

(defconst llm-pick-render-report--right-aligned
  '(score or-in or-out bm-in bm-out gap gain bound)
  "Columns whose cells are aligned to the right.")

(defconst llm-pick-render-report--formats '(table csv)
  "Formats accepted by `llm-pick-render-report-text'.")

;;; The capability bar

(defun llm-pick-render-report--bar (score maximum &optional width)
  "Return a SCORE bar scaled against MAXIMUM, WIDTH cells wide.
WIDTH defaults to `llm-pick-render-report-default-bar-width'.  A nil
SCORE draws an empty bar, so a model without a score still keeps its
column."
  (let* ((width (or width llm-pick-render-report-default-bar-width))
         (filled (if (and (numberp score) (numberp maximum) (> maximum 0))
                     (round (* width (/ (float score) maximum)))
                   0))
         (filled (max 0 (min width filled))))
    (concat (make-string filled ?█)
            (make-string (- width filled) ?░))))

(defun llm-pick-render-report--wrap (line width)
  "Hard-wrap LINE at word boundaries so each line fits WIDTH columns.
Return the wrapped text with lines joined by newlines; continuation
lines get no extra indentation.  If LINE already fits within WIDTH,
return it unchanged."
  (if (<= (length line) width)
      line
    (let ((words (split-string line "[ \t]+" t))
          (parts nil)
          (current nil))
      (dolist (word words)
        (cond
         ((null current)
          (setq current word))
         ((<= (+ (length current) 1 (length word)) width)
          (setq current (concat current " " word)))
         (t
          (push current parts)
          (setq current word))))
      (when (and current (> (length current) 0))
        (push current parts))
      (mapconcat #'identity (nreverse parts) "\n"))))

(defun llm-pick-render-report--maximum-score (models)
  "Return the highest score among MODELS, or nil when none carries one."
  (let (best)
    (dolist (model models best)
      (let ((score (llm-pick-core--field model 'score)))
        (when (and (numberp score) (or (null best) (> score best)))
          (setq best score))))))

;;; Cells

(defun llm-pick-render-report--header (column)
  "Return the header text of COLUMN."
  (or (cdr (assq column llm-pick-render-report--headers))
      (pcase column
        (`(score ,source) (format "Score %s" source))
        (`(score ,source ,category) (format "Score %s %s" source category))
        (`(price ,source ,direction) (format "$/M %s %s" source direction))
        (_ (format "%s" column)))))

(defun llm-pick-render-report--number (value)
  "Return VALUE as a string, without trailing zeroes."
  (if (integerp value)
      (number-to-string value)
    (format "%g" value)))

(defun llm-pick-render-report--percent (ratio)
  "Return RATIO, a fraction, as a percentage cell."
  (if (numberp ratio)
      (format "%.0f%%" (* 100 ratio))
    llm-pick-render-report--missing))

(defun llm-pick-render-report--cell (model column maximum)
  "Return the text of COLUMN for MODEL.
MAXIMUM is the highest score of the report, used to scale the bar."
  (pcase column
    ('bar (llm-pick-render-report--bar (llm-pick-core--field model 'score) maximum))
    ('gap (llm-pick-render-report--percent (llm-pick-core--field model 'gap)))
    ((or 'or-in 'or-out 'bm-in 'bm-out)
     (let ((value (llm-pick-core--field model column))
           (src (if (memq column '(or-in or-out))
                    (symbol-value 'llm-pick-core-default-price-source)
                  (symbol-value 'llm-pick-core-secondary-price-source))))
       (cond ((null value) llm-pick-render-report--missing)
             ((and (numberp value) (zerop value))
              (if (eq src 'openrouter)
                  "free"
                llm-pick-render-report--missing))
             (t (llm-pick-render-report--number value)))))
    (_ (let ((value (llm-pick-core--field model column)))
         (cond ((null value) llm-pick-render-report--missing)
               ((numberp value) (llm-pick-render-report--number value))
               (t (format "%s" value)))))))

(defun llm-pick-render-report--cells (models columns maximum)
  "Return the rows of the COLUMNS of MODELS as a list of cell lists."
  (mapcar (lambda (model)
            (mapcar (lambda (column)
                      (llm-pick-render-report--cell model column maximum))
                    columns))
          models))

(defun llm-pick-render-report--right-aligned-p (column)
  "Return non-nil for a numeric COLUMN."
  (or (memq column llm-pick-render-report--right-aligned)
      (and (consp column) (memq (car column) '(score price)))))

(defun llm-pick-render-report--widths (columns rows)
  "Return the width of every column in COLUMNS, its header included, over ROWS."
  (cl-loop for column in columns
           for index from 0
           collect (let ((width (length (llm-pick-render-report--header column))))
                     (dolist (row rows width)
                       (setq width (max width (length (nth index row))))))))

(defun llm-pick-render-report--pad (text width right)
  "Return TEXT padded to WIDTH, on the left when RIGHT is non-nil."
  (if right
      (concat (make-string (max 0 (- width (length text))) ?\s) text)
    (concat text (make-string (max 0 (- width (length text))) ?\s))))

(defun llm-pick-render-report--line (cells columns widths)
  "Return one table line from CELLS, COLUMNS and WIDTHS."
  (mapconcat #'identity
             (cl-loop for cell in cells
                      for column in columns
                      for width in widths
                      collect (llm-pick-render-report--pad
                               cell width
                               (llm-pick-render-report--right-aligned-p column)))
             "  "))

;;; Formats

(defun llm-pick-render-report--table-from-rows (columns rows)
  "Return a table from COLUMNS and ROWS.
ROWS holds the cells already converted to text, one list per line, so
that a view can build rows that do not come from a single record field."
  (let ((widths (llm-pick-render-report--widths columns rows))
        (header (mapcar #'llm-pick-render-report--header columns)))
    (mapconcat (lambda (cells) (llm-pick-render-report--line cells columns widths))
               (cons header rows)
               "\n")))

(defun llm-pick-render-report-table (models columns)
  "Return MODELS as a table string, one line per model, no trailing newline.
COLUMNS is the list of columns to show, see `llm-pick-render-report--headers'.
A value a model does not carry is shown as `llm-pick-render-report--missing'."
  (llm-pick-render-report--table-from-rows
   columns
   (llm-pick-render-report--cells models columns
                           (llm-pick-render-report--maximum-score models))))

(defun llm-pick-render-report--csv-field (text)
  "Return TEXT quoted as a CSV field when it needs quoting."
  (if (string-match-p "[,\"\n]" text)
      (concat "\"" (replace-regexp-in-string "\"" "\"\"" text) "\"")
    text))

(defun llm-pick-render-report-csv (models columns)
  "Return MODELS as CSV text, one line per model, no trailing newline.
COLUMNS is the list of columns to show, see `llm-pick-render-report--headers'."
  (let* ((maximum (llm-pick-render-report--maximum-score models))
         (header (mapcar (lambda (column)
                           (llm-pick-render-report--csv-field
                            (llm-pick-render-report--header column)))
                         columns))
         (rows (mapcar (lambda (row)
                         (mapcar #'llm-pick-render-report--csv-field row))
                       (llm-pick-render-report--cells models columns maximum))))
    (mapconcat (lambda (fields) (mapconcat #'identity fields ","))
               (cons header rows)
               "\n")))

(defun llm-pick-render-report--gain (gain)
  "Return GAIN, in capability points per dollar, as a cell.
A gain below `llm-pick-render-report-marginal-threshold' is marked with
a `!', because the number alone does not say whether paying more is
worth it."
  (if (numberp gain)
      (let ((text (format "%+.1f" gain)))
        (if (< gain llm-pick-render-report-marginal-threshold)
            (concat text " !")
          text))
    llm-pick-render-report--missing))

(defun llm-pick-render-report--dear-steps (steps)
  "Return the dear ones, or nil.
The STEPS are dear when their gain is below
`llm-pick-render-report-marginal-threshold'."
  (cl-remove-if-not (lambda (step)
                      (let ((gain (cdr step)))
                        (and (numberp gain)
                             (< gain llm-pick-render-report-marginal-threshold))))
                    steps))

(defun llm-pick-render-report--frontier-note (steps)
  "Return the advice for the report, or nil.
The STEPS deserve it when a step buys less than
`llm-pick-render-report-marginal-threshold'."
  (let ((dear (llm-pick-render-report--dear-steps steps)))
    (when dear
      (format "Paying for %s buys less than %.1f points per dollar; a step marked ! is not worth its price."
              (mapconcat (lambda (step)
                           (llm-pick-core--field (car step) 'name))
                         dear " and ")
              llm-pick-render-report-marginal-threshold))))

(defun llm-pick-render-report-frontier (models)
  "Return the Pareto frontier of MODELS as a table.
The frontier is ordered by increasing output price and carries a `gain'
column: the capability points every further dollar buys at that step.
The cheapest model has no step to compare with, so its gain is
`llm-pick-render-report--missing'.  A step that buys less than
`llm-pick-render-report-marginal-threshold' is marked and explained
below the table."
  (let* ((frontier (llm-pick-analyze--frontier models))
         (steps (llm-pick-analyze--marginal frontier))
         (maximum (llm-pick-render-report--maximum-score frontier))
         (table (llm-pick-render-report--table-from-rows
                 '(name bar score or-out gain)
                 (mapcar (lambda (step)
                           (let ((model (car step)))
                             (list (llm-pick-render-report--cell model 'name maximum)
                                   (llm-pick-render-report--cell model 'bar maximum)
                                   (llm-pick-render-report--cell model 'score maximum)
                                   (llm-pick-render-report--cell model 'or-out maximum)
                                   (llm-pick-render-report--gain (cdr step)))))
                         steps)))
         (note (llm-pick-render-report--frontier-note steps)))
    (if note (concat table "\n\n" note) table)))

(defun llm-pick-render-report-budget-guide (models)
  "Render a budget-oriented guide for MODELS.

Aimed at users who have vague expectations about both budget and capability.

MODELS are the post-eligibility models (as filtered by the caller).
The function:
1. computes the Pareto frontier via `llm-pick-analyze--frontier';
2. splits the frontier into three price tiers ($0-1, $1-5, $5+), one
   best model per tier (tier bounds may be parameterized later);
3. per row keeps a capability bar (via `llm-pick-render-report--bar',
   scaled to the highest score), showing: tier label (cheap / mid /
   premium), model name, capability bar, default score, $/M out and a
   value column (score per dollar, the \\='value\\=' field, -- when nil);
4. annotates each frontier step's marginal gain via
   `llm-pick-analyze--step-gain', tagging steps below
   `llm-pick-render-report-marginal-threshold' with \"<- dear step\"
   and high-gain steps with \"<- good step up\";
5. appends two explanatory footer lines: one explaining the columns,
   one giving the conclusion \"For a tight budget pick X; Y adds N
   points for $M more, the best value step\".

Returns a single string (lines joined with newlines, wrapped to 80
columns), like the table renderer, so callers can concat it directly.
All fields are read via `llm-pick-core--field' (price with \\='or-out);
models lacking score/price degrade to ---."
  (let* ((models (append models nil))
         (scored (seq-filter
                  (lambda (m)
                    (and (llm-pick-core--field m 'score)
                         (llm-pick-core--field m 'or-out)))
                  models))
         (max-score (apply #'max 0
                           (mapcar (lambda (m)
                                     (or (llm-pick-core--field m 'score) 0))
                                   scored)))
         (frontier (llm-pick-analyze--frontier scored))
         (bar-width 20)
         (lines (list "Budget guide (Pareto frontier by price tier):" "")))
    (if (null frontier)
        (push "  no eligible models with score and price" lines)
      ;; Group frontier into price tiers and pick best per tier.
      (let* ((tiers
              (list (cons "cheap" 1.0) (cons "mid" 5.0) (cons "premium" nil)))
             (per-tier
              (mapcar
               (lambda (tier)
                 (let* ((label (car tier))
                        (upper (cdr tier))
                        (cands
                         (seq-filter
                          (lambda (m)
                            (let ((p (llm-pick-core--field m 'or-out)))
                              (if upper (and (>= p 0) (< p upper)) t)))
                          frontier)))
                   (cons label (car (last cands)))))
               tiers)))
        (dolist (row per-tier)
          (let* ((label (car row))
                 (m (cdr row)))
            (if (null m)
                (push (format "  %-8s %-20s --" label "(no model)") lines)
              (let* ((score (llm-pick-core--field m 'score))
                     (price (llm-pick-core--field m 'or-out))
                     (value (llm-pick-core--field m 'value))
                     (bar (llm-pick-render-report--bar
                           score max-score bar-width)))
                (push
                 (format "  %-8s %-20s %s %5.1f %6.2f %6s"
                         label
                         (or (llm-pick-core--field m 'name) "---")
                         bar
                         (or score 0.0)
                         (or price 0.0)
                         (if value (format "%.1f" value) "--"))
                 lines))))))
      (push "" lines)
      ;; Step-gain annotations along the frontier.
      (let ((prev nil))
        (dolist (m frontier)
          (when prev
            (let* ((gain (llm-pick-analyze--step-gain prev m))
                   (p-prev (llm-pick-core--field prev 'or-out))
                   (p-cur (llm-pick-core--field m 'or-out))
                   (note (cond ((null gain) nil)
                               ((< gain llm-pick-render-report-marginal-threshold)
                                "  <- dear step")
                               (t "  <- good step up"))))
              (push
               (format "  step %s -> %s: +%s pts for $%.2f more%s"
                       (or (llm-pick-core--field prev 'name) "---")
                       (or (llm-pick-core--field m 'name) "---")
                       (if gain (format "%.1f" gain) "--")
                       (- p-cur p-prev)
                       (or note ""))
               lines)))
          (setq prev m)))
      (push "" lines)
      ;; Conclusion: cheapest frontier model vs its next upgrade.
      (let ((first (car frontier))
            (second (cadr frontier)))
        (push
         (if (and first second
                  (llm-pick-core--field first 'or-out)
                  (llm-pick-core--field second 'or-out))
             (let* ((gain (llm-pick-analyze--step-gain first second))
                    (extra (- (llm-pick-core--field second 'or-out)
                              (llm-pick-core--field first 'or-out))))
               (if gain
                   (format "For a tight budget pick %s; %s adds %d points for $%s more, the best value step"
                           (llm-pick-core--field first 'name)
                           (llm-pick-core--field second 'name)
                           (round gain)
                           (format "%.2f" extra))
                 (format "For a tight budget pick %s"
                         (llm-pick-core--field first 'name))))
           (format "For a tight budget pick %s"
                   (if first (llm-pick-core--field first 'name) "nothing")))
         lines)))
    (push "" lines)
    (push "pt/$ = capability per dollar; a dear step means the dearer model is not worth the extra money"
          lines)
    (mapconcat #'identity
               (nreverse (mapcar (lambda (l) (llm-pick-render-report--wrap l 80)) lines))
               "\n")))

(defun llm-pick-render-report-scatter (models)
  "Render MODELS as a price/capability coordinate plot.
The vertical axis is the score, the horizontal axis the output price
per million tokens on a logarithmic scale.  A star marks a model on
the Pareto frontier, an O a model dominated by one of them.  The
upper left corner is the good corner: more capability for less
money.  Below the plot the frontier models are named with their
score and price, so every star can be looked up."
  (let* ((scored (seq-filter
                  (lambda (m)
                    (let ((s (llm-pick-core--field m 'score))
                          (p (llm-pick-core--field m 'or-out)))
                      (and (numberp s) (numberp p) (> p 0))))
                  (append models nil)))
         (width 56)
         (height 13)
         (frontier (llm-pick-analyze--frontier scored))
         (scores (mapcar (lambda (m) (llm-pick-core--field m 'score)) scored))
         (prices (mapcar (lambda (m) (llm-pick-core--field m 'or-out)) scored))
         (s-lo (floor (apply #'min scores)))
         (s-hi (ceiling (apply #'max scores)))
         (p-lo (apply #'min prices))
         (p-hi (apply #'max prices))
         (grid (mapcar (lambda (_) (make-string width ?\s))
                       (make-list height nil))))
    (if (null scored)
        (list "  no model carries both a score and a positive price")
      (let ((place
             (lambda (m mark)
               (let* ((s (llm-pick-core--field m 'score))
                      (p (llm-pick-core--field m 'or-out))
                      (r (round (* (1- height)
                                   (/ (- s-hi s) (float (- s-hi s-lo))))))
                      (lp (log (max p 1e-9)))
                      (llo (log (max p-lo 1e-9)))
                      (lhi (log (max p-hi 1e-9)))
                      (x (round (+ 1 (* (- width 3)
                                        (/ (- lp llo)
                                           (max (- lhi llo) 1e-9)))))))
                 (aset (nth r grid)
                       (max 1 (min (1- width) x)) mark)))))
        (dolist (m scored)
          (unless (memq m frontier) (funcall place m ?o)))
        (dolist (m frontier) (funcall place m ?*)))
      (append
       (list "  score over price, log scale:"
             (concat "      +" (make-string width ?-)))
       (cl-loop for r from 0 below height
                for s = (- s-hi
                           (round (* r (/ (- s-hi s-lo)
                                          (float (1- height))))))
                collect (format "%5.0f |%s|" s (nth r grid)))
       (list (concat "      +" (make-string width ?-))
             (format "       $%.2f%s$%.2f/M out (log scale)"
                     p-lo
                     (make-string (max 1 (- width 16)) ?\s)
                     p-hi)
             ""
             "  * = Pareto frontier, o = dominated by one of them;"
             "  upper left is the good corner (more capable, cheaper)"
             ""
             (format "  frontier: %s"
                     (mapconcat
                      (lambda (m)
                        (format "%s (%.1f, $%.2f)"
                                (llm-pick-core--field m 'name)
                                (llm-pick-core--field m 'score)
                                (llm-pick-core--field m 'or-out)))
                      frontier ", ")))))))

(defun llm-pick-render-report-bound (bound)
  "Return a price BOUND as a cell; nil is the open ended bucket."
  (if (numberp bound)
      (llm-pick-render-report--number bound)
    "open"))

(defun llm-pick-render-report-ladder (models bounds)
  "Return MODELS bucketed by output price as a table.
BOUNDS lists the upper price of every bucket in USD per million tokens,
the last one nil for the open ended bucket, for instance (0.5 1 2 5
nil).  Every non-empty bucket shows its most capable model."
  (let* ((buckets (llm-pick-analyze--ladder models bounds))
         (maximum (llm-pick-render-report--maximum-score (mapcar #'cdr buckets))))
    (llm-pick-render-report--table-from-rows
     '(bound name score or-out)
     (mapcar (lambda (bucket)
               (let ((model (cdr bucket)))
                 (list (llm-pick-render-report-bound (car bucket))
                       (llm-pick-render-report--cell model 'name maximum)
                       (llm-pick-render-report--cell model 'score maximum)
                       (llm-pick-render-report--cell model 'or-out maximum))))
             buckets))))

(defun llm-pick-render-report-benchmark (models baseline bounds &optional categories)
  "Render a benchmark report comparing MODELS against BASELINE in price BANDS.
BOUNDS is an increasing list of price ratios against BASELINE, the last one nil
for the open ended band.  CATEGORIES names the capability columns to compare;
nil means read the distinct :categories of MODELS, and when that yields nothing
fall back to a single default-score column."
  (let* ((categories (or categories
                         (delete-dups
                          (mapcan (lambda (m)
                                    (copy-sequence (plist-get m :categories)))
                                  models))
                         '(nil)))
         (capability-source (symbol-value 'llm-pick-core-default-capability-source))
         (baseline-name (llm-pick-core--field baseline 'name))
         (baseline-vendor (or (llm-pick-core--field baseline 'vendor)
                              "unknown vendor"))
         (baseline-price (llm-pick-core--field baseline 'or-out))
         (baseline-price-str
          (if (null baseline-price)
              (format "%s (no output price)" llm-pick-render-report--missing)
            (format "$%s/M out" (llm-pick-render-report--number baseline-price))))
         (maximum (cl-loop for m in models
                           maximize (or (llm-pick-core--field m 'or-out) 0)))
         (headline (format "Baseline: %s (%s), %s\n\n"
                           baseline-name baseline-vendor baseline-price-str))
         (bands (llm-pick-analyze--benchmark-bands models baseline bounds))
         (header (append '("Model" "$/M out" "vs base")
                         (mapcar (lambda (cat)
                                   (if cat
                                       (format "Delta %s" cat)
                                     "Delta"))
                                 categories)))
         (align (number-sequence 1 (+ 2 (length categories))))
         (sections
          (cl-loop for (range . band) in bands
                   when band
                   collect
                   (let* ((low (car range))
                          (high (cdr range))
                          (title
                           (cond ((and (null low) (null high)) "any price")
                                 ((null low) (format "< %.2fx" high))
                                 ((null high) (format "%.2fx or more" low))
                                 (t (format "%.2fx to %.2fx" low high))))
                          (rows
                           (cl-loop for model in band
                                    collect
                                    (let* ((model-name (llm-pick-core--field model 'name))
                                           (price-cell (llm-pick-render-report--cell
                                                        model 'or-out maximum))
                                           (ratio (llm-pick-analyze--benchmark-ratio
                                                   model baseline))
                                           (ratio-str (if ratio
                                                          (format "%.2fx" ratio)
                                                        llm-pick-render-report--missing))
                                           (delta-cells
                                            (cl-loop for cat in categories
                                                     for delta = (llm-pick-analyze--benchmark-delta
                                                                  model baseline capability-source cat)
                                                     collect (if (null delta)
                                                                 llm-pick-render-report--missing
                                                               (format "%+.1f" delta)))))
                                      (cons model-name
                                            (cons price-cell
                                                  (cons ratio-str delta-cells)))))))
                     (llm-pick-render-report--section
                      (format "%s (%s)" title
                              (llm-pick-render-report--count (length band) "model"))
                      ""
                      header
                      rows
                      align)))))
    (if sections
        (concat headline (mapconcat #'identity sections "\n\n"))
      (concat headline "No other model falls in any price band.\n"))))

(defun llm-pick-render-report--count (number noun)
  "Return NUMBER and NOUN, pluralized."
  (format "%d %s%s" number noun (if (= number 1) "" "s")))

(defconst llm-pick-render-report--near-miss-band 0.1
  "How far below `llm-pick-align-match-threshold' still counts as a near miss.
An ID this close to a match is the one worth reading: either a
normalization rule is missing, or the threshold is too high.  Everything
further away is, in a live collection, a model the anchor source does not
track at all, and a few hundred of those say nothing.")

(defun llm-pick-render-report--alignment-sources (mapping)
  "Return a hash table of CANONICAL to the sources of MAPPING that name it."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (entry mapping table)
      (let* ((source (car (car entry)))
             (canonical (cdr entry))
             (known (gethash canonical table)))
        (unless (memq source known)
          (puthash canonical (cons source known) table))))))

(defconst llm-pick-render-report--unmatched-headers
  '("Source" "ID as spelled" "Normalizes to" "Closest match" "Score")
  "Header of the table of the IDs that matched no anchor model.")

(defconst llm-pick-render-report--fuzzy-headers
  '("Source" "ID as spelled" "Normalizes to" "Merged into" "Score")
  "Header of the table of the merges a similarity score decided.")

(defconst llm-pick-render-report--agreed-headers
  '("Source" "ID as spelled" "Normalizes to" "Merged into" "Also spelled by")
  "Header of the table of the merges several sources agreed on.")

(defconst llm-pick-render-report--exact-headers
  '("Source" "ID as spelled" "Normalizes to" "Matched")
  "Header of the table of the IDs that normalize to the anchor's model.")

(defun llm-pick-render-report--column-widths (headers rows)
  "Return the width of every column of HEADERS over ROWS, its header included."
  (cl-loop for header in headers
           for index from 0
           collect (let ((width (length header)))
                     (dolist (row rows width)
                       (setq width (max width (length (nth index row))))))))

(defun llm-pick-render-report--rows-table (headers rows right-aligned)
  "Return ROWS as text under HEADERS.
HEADERS and ROWS are lists of strings; RIGHT-ALIGNED names the indexes of
the columns aligned to the right.  Every cell is padded to the width of
its column, so a reader can compare a line with the one above it."
  (let ((widths (llm-pick-render-report--column-widths headers rows)))
    (mapconcat (lambda (cells)
                 (mapconcat #'identity
                            (cl-loop for cell in cells
                                     for index from 0
                                     for width in widths
                                     collect (llm-pick-render-report--pad
                                              cell width
                                              (memq index right-aligned)))
                            "  "))
               (cons headers rows)
               "\n")))

(defun llm-pick-render-report--section (title introduction headers rows right-aligned)
  "Return a report section from TITLE, INTRODUCTION, HEADERS and ROWS.
RIGHT-ALIGNED names the indexes of the columns aligned to the right.
An empty ROWS gives an empty string, so a caller can append every section
of a report unconditionally."
  (if (null rows)
      ""
    (concat "\n" title "\n" introduction "\n\n"
            (llm-pick-render-report--rows-table headers rows right-aligned)
            "\n")))

(defun llm-pick-render-report--entries-of-kind (entries kind)
  "Return the ENTRIES whose :kind is KIND, closest match first."
  (sort (cl-remove-if-not (lambda (entry) (eq (plist-get entry :kind) kind))
                          (copy-sequence entries))
        (lambda (a b)
          (> (or (plist-get a :score) 0.0)
             (or (plist-get b :score) 0.0)))))

(defun llm-pick-render-report--group-by-canonical (entries)
  "Return ENTRIES grouped by canonical ID, ordered by that ID.
The result is a list of (CANONICAL . ENTRY-LIST).  A group is what a
reader has to judge: every ID in it was taken for the model it names, so
one group is one question."
  (let ((table (make-hash-table :test #'equal))
        (order nil))
    (dolist (entry entries)
      (let ((canonical (plist-get entry :canonical)))
        (when (null (gethash canonical table))
          (push canonical order))
        (puthash canonical (append (gethash canonical table) (list entry))
                 table)))
    (mapcar (lambda (canonical) (cons canonical (gethash canonical table)))
            (sort order #'string<))))

(defun llm-pick-render-report--unmatched-rows (entries)
  "Return ENTRIES, unmatched IDs, as table rows."
  (mapcar (lambda (entry)
            (list (format "%s" (plist-get entry :source))
                  (format "%s" (plist-get entry :id))
                  (format "%s" (plist-get entry :norm))
                  (format "%s" (or (plist-get entry :best)
                                   llm-pick-render-report--missing))
                  (if (numberp (plist-get entry :score))
                      (format "%.3f" (plist-get entry :score))
                    llm-pick-render-report--missing)))
          entries))

(defun llm-pick-render-report--fuzzy-rows (entries)
  "Return ENTRIES, IDs a similarity score merged, as table rows."
  (mapcar (lambda (entry)
            (list (format "%s" (plist-get entry :source))
                  (format "%s" (plist-get entry :id))
                  (format "%s" (plist-get entry :norm))
                  (format "%s" (plist-get entry :canonical))
                  (if (numberp (plist-get entry :score))
                      (format "%.3f" (plist-get entry :score))
                    llm-pick-render-report--missing)))
          entries))

(defun llm-pick-render-report--agreed-rows (entries)
  "Return ENTRIES, IDs merged on the word of several sources, as table rows.
The last column names the other sources that spelled the same normalized
ID, so a reader sees the whole group the agreement was made of rather
than one of its members."
  (cl-loop for (canonical . group)
           in (llm-pick-render-report--group-by-canonical entries)
           append (cl-loop for entry in group
                           collect (list (format "%s" (plist-get entry :source))
                                         (format "%s" (plist-get entry :id))
                                         (format "%s" (plist-get entry :norm))
                                         canonical
                                         (mapconcat
                                          (lambda (other)
                                            (format "%s" (plist-get other :source)))
                                          (cl-remove entry group)
                                          ", ")))))

(defun llm-pick-render-report--exact-rows (entries)
  "Return ENTRIES, IDs that normalize to the anchor's model, as table rows.
Nothing was decided for these IDs, which is exactly why they are worth a
reading: a rule of `llm-pick-normalize-rules' that drops too much lands
them on a model that is spelled like them and is not them."
  (mapcar (lambda (entry)
            (list (format "%s" (plist-get entry :source))
                  (format "%s" (plist-get entry :id))
                  (format "%s" (plist-get entry :norm))
                  (format "%s" (plist-get entry :canonical))))
          entries))

(defun llm-pick-render-report-alignment (report)
  "Return REPORT, the plist `llm-pick-align--align' returned, as text.

The report is written to be read by the person who has to accept or
reject what the alignment did, so it names, for every ID it moved, the
source, the ID as that source spells it, the form the normalizer turned
it into and the canonical model it was taken for.  One row is one
decision:

  merged on a similarity score  the ID does not normalize to the anchor's
                                model, so a fuzzy score decided it
  merged on the agreement of
  several sources               no score reached the anchor, but several
                                catalogues spelled the ID alike
  matched no anchor model       either a rule is missing, or the anchor
                                does not track the model

An ID that already normalizes to the anchor's model decided nothing and
is counted, not listed.  Every unmatched ID is listed, whether near the
match threshold or far from it, so a rule that drops too much shows
there, and so does an anchor that does not track a model at all.  The
report the caller passed in is not modified."
  (let* ((threshold (symbol-value 'llm-pick-align-match-threshold))
         (entries (plist-get report :entries))
         (mapping (plist-get report :mapping))
         (canonicals (delete-dups (mapcar #'cdr mapping)))
         (sources (delete-dups (mapcar (lambda (entry) (car (car entry)))
                                       mapping)))
         (by-source (llm-pick-render-report--alignment-sources mapping))
         (shared (cl-count-if (lambda (canonical)
                                (cdr (gethash canonical by-source)))
                              canonicals))
         (exact (llm-pick-render-report--entries-of-kind entries 'exact))
         (fuzzy (llm-pick-render-report--entries-of-kind entries 'fuzzy))
         (agreed (llm-pick-render-report--entries-of-kind entries 'agreed))
         (unmatched (llm-pick-render-report--entries-of-kind entries 'unmatched))
         (merged (append exact fuzzy agreed))
         (merged-in-order
          (cl-remove-if-not
           (lambda (entry)
             (memq (plist-get entry :kind) '(exact fuzzy agreed)))
           entries))
         (by-canonical (when merged-in-order
                         (llm-pick-render-report--group-by-canonical
                          merged-in-order))))
    (concat
     (format "=== ID alignment ===\n\n%s from %s onto %s\n  %s named by more than one source\n  %s named by one source only\n  %s normalizes to the anchor's model already, with nothing to check\n"
             (llm-pick-render-report--count (length mapping) "ID")
             (llm-pick-render-report--count (length sources) "source")
             (llm-pick-render-report--count (length canonicals) "canonical model")
             (llm-pick-render-report--count shared "canonical model")
             (llm-pick-render-report--count (- (length canonicals) shared)
                                     "canonical model")
             (llm-pick-render-report--count (length exact) "ID"))
     (llm-pick-render-report--section
      (format "\n=== Merged on a similarity score (%d) ===" (length fuzzy))
      (mapconcat #'identity
                 '("The ID does not normalize to the anchor's model, so a similarity"
                   "function decided the match.  Read each row: if the ID is not"
                   "another spelling of that model, the merge hides a model behind"
                   "another.")
                 "\n")
      llm-pick-render-report--fuzzy-headers
      (llm-pick-render-report--fuzzy-rows fuzzy) '(4))
     (llm-pick-render-report--section
      (format "\n=== Merged on the agreement of several sources (%d) ==="
              (length agreed))
      (mapconcat #'identity
                 '("No similarity score reached the anchor model here: several sources"
                   "spelled the ID alike by themselves.  Agreement is evidence, not"
                   "proof, so read every group.")
                 "\n")
      llm-pick-render-report--agreed-headers
      (llm-pick-render-report--agreed-rows agreed) '(4))
     (llm-pick-render-report--section
      (format "\n=== Matched no model of the anchor source (%d) ==="
              (length unmatched))
      (mapconcat #'identity
                 (list (format "These are all the unmatched IDs, near the match threshold %.2f or far from it; listed so the normalized form of each can be checked."
                               threshold))
                 "\n")
      llm-pick-render-report--unmatched-headers
      (llm-pick-render-report--unmatched-rows unmatched) '(4))
     (llm-pick-render-report--section
      (format "\n=== Normalized to the anchor's model (%d) ===" (length exact))
      (mapconcat #'identity
                 '("Nothing was decided for these IDs.  They are the place to look"
                   "when a rule of `llm-pick-normalize-rules' drops too much: an ID"
                   "lands here beside a model that is spelled like it and is not it.")
                 "\n")
      llm-pick-render-report--exact-headers
      (llm-pick-render-report--exact-rows exact) nil)
     (when merged
       (concat
        (format "\n=== Canonical models and their merged IDs (%d) ===\n"
                (length by-canonical))
        (mapconcat
         #'identity
         (list
          "The ID marked \"<- main view\" is the best-attested one of that"
          "source: among the IDs the source contributed to this model, exact"
          "normalization outranks agreement among sources, which outranks a"
          "similarity score, and a higher score breaks the tie.  The main"
          "view's provider column shows that ID for its source.  The other"
          "IDs of the same source were merged too, but are not displayed in"
          "the main view.")
         "\n")
        "\n"
        (mapconcat
         (lambda (group)
           (let ((canonical (car group))
                 (group-entries (cdr group)))
             (concat
              (format "%s  %s"
                      canonical
                      (llm-pick-render-report--count (length group-entries)
                                                     "ID"))
              (let ((best (make-hash-table :test #'equal))
                    (quality (make-hash-table :test #'equal)))
                ;; First pass: decide the best-attested entry per source.
                (dolist (entry group-entries)
                  (let* ((source (plist-get entry :source))
                         (q (+ (pcase (plist-get entry :kind)
                                 ('exact 3) ('agreed 2) ('fuzzy 1)
                                 (_ 0))
                               (or (plist-get entry :score) 0))))
                    (when (and source
                               (or (not (gethash source quality))
                                   (> q (gethash source quality))))
                      (puthash source q quality)
                      (puthash source entry best))))
                ;; Second pass: format each line, marking only the entry
                ;; that survived the first pass for its source.
                (mapconcat
                 (lambda (entry)
                   (let ((source (plist-get entry :source)))
                     (format "\n  %s (%s%s)%s"
                             (plist-get entry :id)
                             (plist-get entry :kind)
                             (if (plist-get entry :score)
                                 (format ", %.2f" (plist-get entry :score))
                               "")
                             (if (and source
                                      (eq entry (gethash source best)))
                                 " <- main view"
                               ""))))
                 group-entries
                 "")))))
         by-canonical
         "\n"))))))

(defun llm-pick-render-report--indent (text columns)
  "Return TEXT with every line indented by COLUMNS spaces."
  (let ((prefix (make-string columns ?\s)))
    (concat prefix (replace-regexp-in-string "\n" (concat "\n" prefix) text))))

(defun llm-pick-render-report--problem-types (problems)
  "Return the kinds of PROBLEMS, each once, in the order they appeared."
  (let (types)
    (dolist (problem problems (nreverse types))
      (let ((type (plist-get problem :type)))
        (unless (memq type types)
          (push type types))))))

(defun llm-pick-render-report-text (models columns &optional format)
  "Return MODELS rendered as text in FORMAT.
COLUMNS is the list of columns to show, see `llm-pick-render-report--headers'.
FORMAT is `table' (the default) or `csv', see `llm-pick-render-report-table'
and `llm-pick-render-report-csv'.  Signal `llm-pick-error' for anything else."
  (pcase (or format 'table)
    ('table (llm-pick-render-report-table models columns))
    ('csv (llm-pick-render-report-csv models columns))
    (_ (signal 'llm-pick-error
               (list (format "Unknown render format: %S, expected one of %S"
                             format llm-pick-render-report--formats))))))

(provide 'llm-pick-render-report)

;;; llm-pick-render-report.el ends here
