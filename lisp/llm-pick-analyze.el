;;; llm-pick-analyze.el --- Pareto frontier and marginal gain -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: Claude
;; URL: https://github.com/OverbearingPearl/llm-pick
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;
;; Capability and price are the two axes of the choice.  A model is on
;; the Pareto frontier when no other model is at least as capable for at
;; most the same price; the frontier is what the user actually chooses
;; from.  Walking the frontier by increasing price gives the marginal
;; gain, in points per dollar, of every step up, which is the number that
;; answers "is the expensive model worth it?".
;;
;; Every function here is pure with respect to the model records.
;; Capability and price are read through `llm-pick-core--field', so the caller
;; has to keep `llm-pick-core-default-capability-source' and
;; `llm-pick-core-default-price-source' pointed at the sources the records
;; were collected from.

;;; Code:

(require 'cl-lib)
(require 'llm-pick-core)

(defun llm-pick-analyze--comparable-p (model)
  "Return non-nil when MODEL carries both a score and an output price.
Make sure both values are numbers."
  (let ((score (llm-pick-core--field model 'score))
        (price (llm-pick-core--field model 'or-out)))
    (and (numberp score) (numberp price))))

(defun llm-pick-analyze--dominates-p (a b)
  "Return non-nil when the price/capability trade of A beats that of B.
A dominates B when A is at least as capable, costs no more, and is
strictly better on one of the two axes."
  (let ((score-a (llm-pick-core--field a 'score))
        (score-b (llm-pick-core--field b 'score))
        (price-a (llm-pick-core--field a 'or-out))
        (price-b (llm-pick-core--field b 'or-out)))
    (and (numberp score-a) (numberp score-b)
         (numberp price-a) (numberp price-b)
         (>= score-a score-b)
         (<= price-a price-b)
         (or (> score-a score-b) (< price-a price-b)))))

(defun llm-pick-analyze--frontier (models)
  "Return the Pareto frontier of MODELS, ordered by increasing price.
Models without a score or without a price are ignored.  MODELS itself
keeps its order: the sort runs on a copy, because `cl-remove-if'
returns its argument when it removes nothing and `sort' reorders a list
in place."
  (let* ((models (copy-sequence models))
         (usable (cl-remove-if-not #'llm-pick-analyze--comparable-p models)))
    (sort (cl-remove-if (lambda (model)
                          (cl-some (lambda (other)
                                     (and (not (eq other model))
                                          (llm-pick-analyze--dominates-p other model)))
                                   usable))
                        usable)
          (lambda (a b) (< (llm-pick-core--field a 'or-out)
                           (llm-pick-core--field b 'or-out))))))

(defun llm-pick-analyze--step-gain (from to)
  "Return the capability gained per dollar when moving from FROM to TO.
Nil when FROM is nil or when the two models cost the same."
  (when from
    (let ((score (llm-pick-core--field to 'score))
          (from-score (llm-pick-core--field from 'score))
          (price (llm-pick-core--field to 'or-out))
          (from-price (llm-pick-core--field from 'or-out)))
      (when (and (numberp score) (numberp from-score)
                 (numberp price) (numberp from-price)
                 (> (- price from-price) 0))
        (/ (- score from-score) (- price from-price))))))

(defun llm-pick-analyze--marginal (models)
  "Return the marginal gain of every step along MODELS.
MODELS is ordered by increasing price, as `llm-pick-analyze--frontier' returns
it.  Return a list of (MODEL . POINTS-PER-DOLLAR); the first element has
a nil gain because there is nothing to compare it with."
  (let ((result nil)
        (previous nil))
    (dolist (model models)
      (push (cons model (llm-pick-analyze--step-gain previous model)) result)
      (setq previous model))
    (nreverse result)))

(defun llm-pick-analyze--next-upgrade (models model)
  "Return the cheapest model of MODELS that is more capable than MODEL.
MODELS itself keeps its order; the sort runs on a copy."
  (let ((score (llm-pick-core--field model 'score)))
    (car (sort (cl-remove-if-not
                (lambda (candidate)
                  (let ((candidate-score (llm-pick-core--field candidate 'score)))
                    (and (not (eq candidate model))
                         (numberp candidate-score)
                         (numberp (llm-pick-core--field candidate 'or-out))
                         (> candidate-score score))))
                (copy-sequence models))
               (lambda (a b) (< (llm-pick-core--field a 'or-out)
                                (llm-pick-core--field b 'or-out)))))))

(defun llm-pick-analyze--bucket-index (price bounds)
  "Return the index of the bucket of PRICE in BOUNDS.
BOUNDS is a list of increasing upper bounds, the last one nil for the
open ended bucket.  A price equal to a bound belongs to the bucket above
it."
  (cl-loop for bound in bounds
           for index from 0
           when (or (null bound) (< price bound))
           return index
           finally return (1- (length bounds))))

(defun llm-pick-analyze--better-than-p (model other)
  "Return non-nil when MODEL is more capable than OTHER.
A nil OTHER counts as beaten; a model without a score never wins."
  (or (null other)
      (let ((score (llm-pick-core--field model 'score))
            (other-score (llm-pick-core--field other 'score)))
        (and (numberp score) (> score (or other-score -1))))))

(defun llm-pick-analyze--ladder (models bounds)
  "Bucket MODELS by output price using BOUNDS.
BOUNDS lists the upper price of every bucket in USD per million tokens,
the last one nil for the open ended bucket, for instance (0.5 1 2 nil).
Return an alist (BOUND . MODEL) holding the most capable model of every
non-empty bucket, in the order of BOUNDS."
  (let ((buckets (mapcar (lambda (_bound) (list nil)) bounds)))
    (dolist (model models)
      (let ((price (llm-pick-core--field model 'or-out)))
        (when (numberp price)
          (let ((bucket (nth (llm-pick-analyze--bucket-index price bounds) buckets)))
            (when (llm-pick-analyze--better-than-p model (car bucket))
              (setcar bucket model))))))
    (cl-loop for bound in bounds
             for bucket in buckets
             when (car bucket)
             collect (cons bound (car bucket)))))

;;; Benchmarking against one model

(defun llm-pick-analyze--benchmark-ratio (model baseline)
  "Return MODEL's output price as a multiple of BASELINE's.
Return nil when either model lacks an OR-OUT price or when
BASELINE costs nothing."
  (let ((price (llm-pick-core--field model 'or-out))
        (base (llm-pick-core--field baseline 'or-out)))
    (when (and price base (not (zerop base)))
      (/ price base))))

(defun llm-pick-analyze--benchmark-distance (model baseline)
  "Return how far MODEL's price ratio from BASELINE is from one.
A result of zero means the same price, and one means MODEL costs
twice as much or half as much as BASELINE."
  (let ((ratio (llm-pick-analyze--benchmark-ratio model baseline)))
    (when ratio
      (abs (- ratio 1)))))

(defun llm-pick-analyze--benchmark-range (index bounds)
  "Return the price-ratio range of the band at INDEX in BOUNDS.
LOW is nil for the open bottom band and HIGH is nil for the open
ended band, so a renderer can name a band without knowing whether
it is the first or the last one."
  (cons (when (> index 0)
          (nth (1- index) bounds))
        (nth index bounds)))

(defun llm-pick-analyze--benchmark-bands (models baseline bounds)
  "Return MODELS bucketed into price bands around BASELINE using BOUNDS.
Each element is (RANGE . MODEL-LIST) for a non-empty band, in the
order of BOUNDS.  Models are sorted by distance from BASELINE, and
BASELINE itself and models without a price are omitted."
  (let ((buckets (mapcar (lambda (_) nil) bounds)))
    (dolist (model models)
      (unless (equal model baseline)
        (let ((ratio (llm-pick-analyze--benchmark-ratio model baseline)))
          (when (numberp ratio)
            (let* ((index (llm-pick-analyze--bucket-index ratio bounds))
                   (cell (nthcdr index buckets)))
              (setcar cell (cons model (car cell))))))))
    (let (result)
      (dotimes (index (length bounds))
        (let* ((bucket (nth index buckets))
               (sorted (sort bucket
                             (lambda (a b)
                               (< (abs (- (llm-pick-analyze--benchmark-ratio
                                           a baseline)
                                          1))
                                  (abs (- (llm-pick-analyze--benchmark-ratio
                                           b baseline)
                                          1)))))))
          (when sorted
            (push (cons (llm-pick-analyze--benchmark-range index bounds)
                        sorted)
                  result))))
      (nreverse result))))

(defun llm-pick-analyze--benchmark-delta (model baseline source category)
  "Return MODEL's capability points above BASELINE for SOURCE and CATEGORY.
When CATEGORY is nil, read the default score column instead.  A
negative result means MODEL is weaker, and nil is returned when
either model lacks that score."
  (let* ((column (if category
                     (list 'score source category)
                   'score))
         (score (llm-pick-core--field model column))
         (base (llm-pick-core--field baseline column)))
    (when (and score base)
      (- score base))))

(provide 'llm-pick-analyze)

;;; llm-pick-analyze.el ends here
