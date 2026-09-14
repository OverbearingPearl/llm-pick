;;; llm-pick-core.el --- Data model, field access and predicates -*- lexical-binding: t; -*-

;; Copyright (C) 2025 madachuan
;; Author: madachuan <madachuan.noreply.github.com>
;; Assisted-by: Claude
;; URL: https://github.com/madachuan/llm-pick
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;
;; A model record is a plist built by `llm-pick-core--make-record':
;;
;;   :canonical     canonical ID, unique inside llm-pick
;;   :display-name  human readable name
;;   :scores        alist (SOURCE . SCORE), SCORE is 0-100; a record
;;                  collected for several categories keys them by
;;                  (SOURCE . CATEGORY) instead
;;   :prices        alist (SOURCE :in PRICE :out PRICE), USD per million
;;                  tokens
;;   :providers     alist (PROVIDER . ID)
;;   :categories    the categories the record was collected for
;;   :scope         both, capability-only, price-only or unknown
;;
;; Every other module reads model data through `llm-pick-core--field' instead
;; of poking at the plist keys, so that a field can be computed rather
;; than stored: `value' and `gap' are such fields.

;;; Code:

(require 'cl-lib)
(require 'json)

;; The user options live in llm-pick.el, which requires this module.
;; Declaring them here keeps byte-compilation free of free-variable
;; warnings without moving the user interface out of the main module.
(defvar llm-pick-core-default-capability-source)
(defvar llm-pick-core-default-price-source)
(defvar llm-pick-core-secondary-price-source)

;;; Errors

(define-error 'llm-pick-error "llm-pick error")

;;; JSON

(defun llm-pick-core--parse-json (text)
  "Return the JSON TEXT as hash tables, lists and scalars.
Objects become hash tables with string keys, and JSON null and false
become nil.  Signal `llm-pick-error' for TEXT that is not JSON, naming
the problem: a service that answers an error page must not look like a
source that had nothing to say."
  (condition-case err
      (json-parse-string text
                         :object-type 'hash-table
                         :array-type 'list
                         :null-object nil
                         :false-object nil)
    (json-parse-error
     (signal 'llm-pick-error
             (list (format "Cannot read the JSON answer: %s"
                           (error-message-string err)))))))

;;; Data model

(defun llm-pick-core--make-record (canonical &rest args)
  "Return a new model record for CANONICAL.
ARGS is a plist of fields merged into the record, for instance
:display-name, :scores, :prices, :providers and :scope."
  (let ((record (list :canonical canonical
                      :display-name canonical
                      :scores nil
                      :prices nil
                      :providers nil
                      :categories nil
                      :scope 'unknown)))
    (cl-loop for (key value) on args by #'cddr
             do (setq record (plist-put record key value)))
    record))

;;; Field access

(defun llm-pick-core--field (m field)
  "Return the value of FIELD in the model record M.
FIELD is the symbol `name', `provider', `scope', `score', `or-in',
`or-out', `bm-in', `bm-out', `gap' or `value', or a list of the form
\(score SOURCE), \(score SOURCE CATEGORY) or \(price SOURCE DIRECTION).
The value is nil when M does not carry the requested information."
  (pcase field
    ('name (plist-get m :canonical))
    ('provider (caar (plist-get m :providers)))
    ('scope (plist-get m :scope))
    ('score (llm-pick-core--default-score m))
    ('or-in (llm-pick-core--price m llm-pick-core-default-price-source 'in))
    ('or-out (llm-pick-core--price m llm-pick-core-default-price-source 'out))
    ('bm-in (llm-pick-core--price m llm-pick-core-secondary-price-source 'in))
    ('bm-out (llm-pick-core--price m llm-pick-core-secondary-price-source 'out))
    ('gap (llm-pick-core--price-gap m))
    ('value (llm-pick-core--value m))
    (`(score ,source) (llm-pick-core--score m source nil))
    (`(score ,source ,category) (llm-pick-core--score m source category))
    (`(price ,source ,direction) (llm-pick-core--price m source direction))
    (_ (error "Unknown field: %S" field))))

(defun llm-pick-core--category-name (category)
  "Return CATEGORY as the name a record keys its scores by.
A symbol reads as its name, so `(score benchlm coding)' and
`(score benchlm \"coding\")' select the same column.  Nil stays nil:
it means the best score of any category, and `symbolp' would turn it
into the name \"nil\" and lose the score of every model."
  (cond ((null category) nil)
        ((symbolp category) (symbol-name category))
        (t category)))

(defun llm-pick-core--score (m source category)
  "Return the capability score of M at SOURCE.
CATEGORY picks one of the categories a record collected for several
carries; nil reads the score of a record collected for one."
  (let ((scores (plist-get m :scores)))
    (if category
        (cdr (assoc (cons source (llm-pick-core--category-name category)) scores))
      (cdr (assoc source scores)))))

(defun llm-pick-core--default-score (m)
  "Return the score of M at `llm-pick-core-default-capability-source'.
A record collected for several categories keys its scores by
\(SOURCE . CATEGORY\), so the first category of that source wins."
  (let ((source llm-pick-core-default-capability-source)
        (scores (plist-get m :scores)))
    (or (cdr (assoc source scores))
        (cl-loop for (key . value) in scores
                 when (and (consp key) (eq (car key) source))
                 return value))))

(defun llm-pick-core--direction-key (direction)
  "Return the key under which the price of DIRECTION is kept in a price plist."
  (pcase direction
    ('in :in)
    ('out :out)
    (_ (error "Unknown price direction: %S" direction))))

(defun llm-pick-core--price (m source direction)
  "Return the DIRECTION price of M at SOURCE, in USD per million tokens.
DIRECTION is `in' or `out'."
  (plist-get (alist-get source (plist-get m :prices))
             (llm-pick-core--direction-key direction)))

(defun llm-pick-core--price-gap (m)
  "Return how much the two price sources of M disagree, as a ratio.
The cheaper of the two output prices is the baseline, so the result is
0 when the sources agree, 0.25 when the dearer one costs a quarter
more, and nil when M lacks either price.  nil is also the answer while
no second price source is registered, see
`llm-pick-core-secondary-price-source'."
  (let ((first (llm-pick-core--price m llm-pick-core-default-price-source 'out))
        (second (llm-pick-core--price m llm-pick-core-secondary-price-source 'out)))
    (when (and (numberp first) (numberp second)
               (> first 0) (> second 0))
      (/ (abs (- first second)) (float (min first second))))))

(defun llm-pick-core--value (m)
  "Return the capability per dollar of M, or nil when it is unknown."
  (let ((score (llm-pick-core--default-score m))
        (price (llm-pick-core--price m llm-pick-core-default-price-source 'out)))
    (when (and (numberp score) (numberp price) (> price 0))
      (/ score price))))

;;; Predicates

(defun llm-pick-core--text (value)
  "Return VALUE as the text a predicate compares against.
A symbol field such as `provider' reads as its name, so that
`(provider ~ \"anthropic\")' tests what the user meant instead of
failing on a type it never sees."
  (if (stringp value) value (format "%s" value)))

(defun llm-pick-core--equal-field (m field value)
  "Return non-nil when FIELD of M equals VALUE.
Numbers compare as numbers, everything else as text, so that
`(provider = \"google\")' matches the provider symbol `google'."
  (let ((actual (llm-pick-core--field m field)))
    (cond ((null actual) nil)
          ((and (numberp actual) (numberp value)) (= actual value))
          (t (equal (llm-pick-core--text actual) (llm-pick-core--text value))))))

(defun llm-pick-core--compare (m field value op)
  "Return non-nil when the numeric FIELD of M compares to VALUE by OP."
  (let ((actual (llm-pick-core--field m field)))
    (and (numberp actual) (numberp value) (funcall op actual value))))

(defun llm-pick-core--match-p (m pred)
  "Return non-nil when the model record M satisfies PRED.
PRED is a list \(OP FIELD ARG) where OP is `>', `<', `>=', `<=', `=',
`~' (regexp match) or `in' (membership).  A field M does not carry
never matches, it does not signal.  `=' and `~' read a symbol field
such as `provider' as its name, so \`(provider = \"google\")' matches."
  (pcase pred
    (`(> ,field ,value) (llm-pick-core--compare m field value #'>))
    (`(< ,field ,value) (llm-pick-core--compare m field value #'<))
    (`(>= ,field ,value) (llm-pick-core--compare m field value #'>=))
    (`(<= ,field ,value) (llm-pick-core--compare m field value #'<=))
    (`(= ,field ,value) (llm-pick-core--equal-field m field value))
    (`(~ ,field ,regexp)
     (let ((value (llm-pick-core--field m field)))
       (and value
            (string-match-p regexp (llm-pick-core--text value))
            t)))
    (`(in ,field ,values)
     (and (member (llm-pick-core--field m field) values) t))
    (_ (error "Unknown predicate: %S" pred))))

(defun llm-pick-core--filter (models where)
  "Return the models among MODELS that satisfy every predicate in WHERE.
WHERE is a list of predicates, see `llm-pick-core--match-p'.  A nil WHERE
returns MODELS unchanged."
  (if (null where)
      models
    (cl-remove-if-not
     (lambda (model)
       (cl-every (lambda (pred) (llm-pick-core--match-p model pred)) where))
     models)))

(provide 'llm-pick-core)

;;; llm-pick-core.el ends here
