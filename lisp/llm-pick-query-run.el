;;; llm-pick-query-run.el --- Select, sort and trim model records -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: madachuan <madachuan.noreply.github.com>
;; Assisted-by: Claude
;; URL: https://github.com/madachuan/llm-pick
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;
;; The four questions a report answers are orthogonal:
;;
;;   what to show     :columns and :format, see llm-pick-render
;;   what to keep     :where, :scope, :available-on (:providers), :budget,
;;                    :target-score
;;   where data comes from  :category, :sources, see llm-pick-collect
;;   in which order   :order, :descending, :top
;;
;; This module owns the two middle lines.  It takes the records
;; `llm-pick-collect' returned and returns the records the user asked
;; for; everything here is pure.
;;
;; Price means the output price, the same axis `llm-pick-core--value', the
;; Pareto frontier and the price ladder use.  A model that lacks the
;; field a criterion needs is dropped, never guessed at: a model without
;; a price cannot be shown to be affordable.

;;; Code:

(require 'cl-lib)
(require 'llm-pick-core)

(defconst llm-pick-query-run--keys
  '(:where :scope :available-on :budget :target-score :order :descending :top)
  "Arguments accepted by `llm-pick-query-run-query'.")

(defconst llm-pick-query-run--aliases
  '((:providers . :available-on))
  "Argument spellings accepted in place of the canonical names.
Both spellings of an argument work, so that a caller written against
either one keeps going; a key that is not in `llm-pick-query-run--keys'
after the rename is still an error.")

(defun llm-pick-query-run--expand-aliases (args)
  "Return ARGS with every alias of `llm-pick-query-run--aliases' renamed."
  (cl-loop for (key value) on args by #'cddr
           append (list (or (cdr (assq key llm-pick-query-run--aliases)) key)
                        value)))

(defun llm-pick-query-run--check (args)
  "Signal `llm-pick-error' when ARGS is not a valid argument plist."
  (unless (cl-evenp (length args))
    (signal 'llm-pick-error (list (format "Odd number of arguments: %S" args))))
  (cl-loop for (key _value) on args by #'cddr
           unless (memq key llm-pick-query-run--keys)
           do (signal 'llm-pick-error
                      (list (format "Unknown query argument: %S, expected one of %S"
                                    key llm-pick-query-run--keys)))))

;;; Criteria

(defun llm-pick-query-run--scope-p (model scopes)
  "Return non-nil when MODEL has one of the SCOPES symbols."
  (memq (llm-pick-core--field model 'scope) scopes))

(defun llm-pick-query-run--available-on-p (model providers)
  "Return non-nil when MODEL carries an ID for one of PROVIDERS."
  (let ((known (mapcar #'car (plist-get model :providers))))
    (cl-some (lambda (provider) (memq provider known)) providers)))

(defun llm-pick-query-run--affordable-p (model budget)
  "Return non-nil when MODEL costs at most BUDGET per million output tokens."
  (let ((price (llm-pick-core--field model 'or-out)))
    (and (numberp price) (<= price budget))))

(defun llm-pick-query-run--capable-p (model target)
  "Return non-nil when MODEL scores at least TARGET."
  (let ((score (llm-pick-core--field model 'score)))
    (and (numberp score) (>= score target))))

(defun llm-pick-query-run--keep (models predicate argument)
  "Return the models of MODELS that PREDICATE accepts with ARGUMENT.
A nil ARGUMENT keeps everything."
  (if (null argument)
      models
    (cl-remove-if-not (lambda (model) (funcall predicate model argument))
                      models)))

;;; Order

(defun llm-pick-query-run--before-p (a b order descending)
  "Return non-nil when A orders before B by ORDER.
DESCENDING means sort in descending order.
A model without the field sorts last, in either direction."
  (let ((value-a (llm-pick-core--field a order))
        (value-b (llm-pick-core--field b order)))
    (cond ((null value-a) nil)
          ((null value-b) t)
          ((equal value-a value-b) nil)
          ((and (numberp value-a) (numberp value-b))
           (if descending (> value-a value-b) (< value-a value-b)))
          ((if descending
               (string> (format "%s" value-a) (format "%s" value-b))
             (string< (format "%s" value-a) (format "%s" value-b)))))))

(defun llm-pick-query-run--sorted (models order descending)
  "Return MODELS sorted by ORDER, descending when DESCENDING is non-nil."
  (if (null order)
      models
    (sort (copy-sequence models)
          (lambda (a b) (llm-pick-query-run--before-p a b order descending)))))

(defun llm-pick-query-run--top (models top)
  "Return at most the first TOP models of MODELS."
  (if (null top)
      models
    (cl-subseq models 0 (min top (length models)))))

;;; Entry point

(defun llm-pick-query-run-query (models &rest args)
  "Return the models of MODELS that ARGS asks for.
MODELS is a list of records as `llm-pick-collect' returns them.  ARGS is
a plist:

  :where         predicates every model has to satisfy, see
                 `llm-pick-core--match-p'
  :scope         scope symbol, or a list of them
  :available-on  providers; a model is kept when it carries an ID for at
                 least one of them
  :budget        highest output price in USD per million tokens
  :target-score  lowest capability score
  :order         field to sort by, see `llm-pick-core--field'
  :descending    non-nil reverses the sort order
  :top           keep at most this many models

An unknown argument is an error, not a silently ignored key, and
`:providers' is accepted as a spelling of `:available-on'."
  (setq args (llm-pick-query-run--expand-aliases args))
  (llm-pick-query-run--check args)
  (let ((result models)
        (scope (plist-get args :scope))
        (providers (plist-get args :available-on)))
    (setq result (llm-pick-core--filter result (plist-get args :where)))
    (when scope
      (setq result (llm-pick-query-run--keep result #'llm-pick-query-run--scope-p
                                         (if (listp scope) scope (list scope)))))
    (when providers
      (setq result (llm-pick-query-run--keep result #'llm-pick-query-run--available-on-p
                                         providers)))
    (setq result (llm-pick-query-run--keep result #'llm-pick-query-run--affordable-p
                                       (plist-get args :budget)))
    (setq result (llm-pick-query-run--keep result #'llm-pick-query-run--capable-p
                                       (plist-get args :target-score)))
    (llm-pick-query-run--top
     (llm-pick-query-run--sorted result (plist-get args :order)
                             (plist-get args :descending))
     (plist-get args :top))))

(provide 'llm-pick-query-run)

;;; llm-pick-query-run.el ends here
