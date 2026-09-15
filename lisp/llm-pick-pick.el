;;; llm-pick-pick.el --- Choose one model and resolve a provider ID -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: madachuan <madachuan.noreply.github.com>
;; Assisted-by: Claude
;; URL: https://github.com/madachuan/llm-pick
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;
;; A report helps the user choose; this module chooses.  The rules are
;; deliberately narrow, so that the answer is predictable:
;;
;;   with a budget       the most capable model that fits
;;   with a target score the cheapest model that reaches it
;;   with both           the most capable model that satisfies both
;;
;; Capability decides and price breaks the tie, which is why
;; `llm-pick-pick--choose' sorts by capability and then by price and takes
;; the head.
;;
;; A choice is a canonical ID, llm-pick's own key.  Every API wants the
;; ID its provider uses, so `llm-pick-pick--resolve' maps a canonical ID back
;; to its (PROVIDER . ID) pair, and `llm-pick-pick--resolve-with-fallback'
;; walks a list of providers until one of them names the model.
;;
;; Only a model that carries both a score and an output price can be
;; chosen: without a price it cannot be shown to be affordable, and
;; without a score it cannot be shown to be the most capable.  An empty
;; result is an error, never nil, so a caller does not have to tell
;; "nothing fits" apart from a bug.

;;; Code:

(require 'cl-lib)
(require 'llm-pick-core)
(require 'llm-pick-query-run)
(require 'llm-pick-analyze)

;;; Resolving

(defun llm-pick-pick--find (models canonical)
  "Return the record of MODELS whose canonical ID is CANONICAL, or nil."
  (car (cl-remove-if-not (lambda (model)
                           (equal (llm-pick-core--field model 'name) canonical))
                         models)))

(defun llm-pick-pick--providers (record)
  "Return the (PROVIDER . ID) alist of RECORD."
  (plist-get record :providers))

(defun llm-pick-pick--known-providers (record)
  "Return the provider names of RECORD, or a note that it names none."
  (or (mapcar #'car (llm-pick-pick--providers record))
      "no provider at all"))

(defun llm-pick-pick--resolve (models canonical provider)
  "Return the (PROVIDER . ID) pair of CANONICAL at PROVIDER.
Signal `llm-pick-error' when no record of MODELS carries CANONICAL, or
when that record has no ID for PROVIDER."
  (let* ((record (llm-pick-pick--find models canonical))
         (id (and record (alist-get provider (llm-pick-pick--providers record)))))
    (cond
     ((null record)
      (signal 'llm-pick-error
              (list (format "No model has the canonical ID %S" canonical))))
     (id (cons provider id))
     (t (signal 'llm-pick-error
                (list (format "%S has no ID for %S; it is known as %s"
                              canonical provider
                              (llm-pick-pick--known-providers record))))))))

(defun llm-pick-pick--resolve-with-fallback (models canonical providers)
  "Return the first (PROVIDER . ID) pair of CANONICAL among PROVIDERS.
PROVIDERS is tried in order.  Signal `llm-pick-error' when no record of
MODELS carries CANONICAL, or when none of PROVIDERS names it."
  (let ((record (llm-pick-pick--find models canonical)))
    (unless record
      (signal 'llm-pick-error
              (list (format "No model has the canonical ID %S" canonical))))
    (or (cl-loop for provider in providers
                 for id = (alist-get provider (llm-pick-pick--providers record))
                 when id return (cons provider id))
        (signal 'llm-pick-error
                (list (format "None of %S names %S; it is known as %s"
                              providers canonical
                              (llm-pick-pick--known-providers record)))))))

;;; Choosing

(defun llm-pick-pick--better-p (a b)
  "Return non-nil when A is the better pick than B.
The more capable model wins; on equal scores the cheaper one wins."
  (let ((score-a (llm-pick-core--field a 'score))
        (score-b (llm-pick-core--field b 'score)))
    (cond ((> score-a score-b) t)
          ((< score-a score-b) nil)
          (t (< (llm-pick-core--field a 'or-out)
                (llm-pick-core--field b 'or-out))))))

(defun llm-pick-pick--cheaper-p (a b)
  "Return non-nil when A is the cheaper pick than B.
The cheaper model wins; on equal prices the more capable one wins."
  (let ((price-a (llm-pick-core--field a 'or-out))
        (price-b (llm-pick-core--field b 'or-out)))
    (cond ((< price-a price-b) t)
          ((> price-a price-b) nil)
          (t (> (llm-pick-core--field a 'score)
                (llm-pick-core--field b 'score))))))

(defun llm-pick-pick--criteria (args)
  "Return the selection criteria of ARGS as readable text."
  (let ((budget (plist-get args :budget))
        (target (plist-get args :target-score))
        (providers (plist-get args :available-on))
        parts)
    (when budget
      (push (format "an output price of at most $%s/M" budget) parts))
    (when target
      (push (format "a score of at least %s" target) parts))
    (when providers
      (push (format "availability on %S" providers) parts))
    (if parts
        (mapconcat #'identity (nreverse parts) " and ")
      "no criterion")))

(defun llm-pick-pick--choose (models &rest args)
  "Return the model of MODELS that ARGS asks for.
ARGS is a plist:

  :budget        highest output price in USD per million tokens
  :target-score  lowest capability score
  :available-on  providers; the result carries an ID for at least one of
                 them

Only models that carry both a score and an output price take part, and
the ranking follows the criterion that was given:

  a budget alone        the most capable model that fits
  a target score alone  the cheapest model that reaches the target
  both                  the most capable model that satisfies both

A tie on the first criterion is broken by the other one.  Signal
`llm-pick-error' when nothing qualifies."
  (let* ((budget (plist-get args :budget))
         (candidates (llm-pick-query-run-query
                      models
                      :budget budget
                      :target-score (plist-get args :target-score)
                      :available-on (plist-get args :available-on)))
         ;; `llm-pick-query-run-query' hands back its argument when it removes
         ;; nothing, so the sort below has to run on a copy.
         (usable (cl-remove-if-not #'llm-pick-analyze--comparable-p
                                   (copy-sequence candidates))))
    (unless usable
      (signal 'llm-pick-error
              (list (format "No model among the %d known ones satisfies %s"
                            (length models) (llm-pick-pick--criteria args)))))
    (car (sort usable (if (and (plist-get args :target-score) (null budget))
                          #'llm-pick-pick--cheaper-p
                        #'llm-pick-pick--better-p)))))

(provide 'llm-pick-pick)

;;; llm-pick-pick.el ends here
