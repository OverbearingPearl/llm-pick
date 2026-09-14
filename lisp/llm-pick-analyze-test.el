;;; llm-pick-analyze-test.el --- Tests for llm-pick-analyze -*- lexical-binding: t; -*-

;;; Commentary:

;;
;; The frontier, the marginal gain and the price ladder are tested on
;; synthetic records so that the expectations can be checked by hand.

;;; Code:

(require 'ert)
(require 'llm-pick-analyze)

(defconst llm-pick-analyze-test--models
  (list (llm-pick-core--make-record "weak-cheap"
                               :scores '((benchlm . 60))
                               :prices '((openrouter :in 0.1 :out 0.5)))
        (llm-pick-core--make-record "mid"
                               :scores '((benchlm . 75))
                               :prices '((openrouter :in 0.2 :out 1.0)))
        (llm-pick-core--make-record "strong"
                               :scores '((benchlm . 85))
                               :prices '((openrouter :in 0.5 :out 2.0)))
        (llm-pick-core--make-record "dominated"
                               :scores '((benchlm . 70))
                               :prices '((openrouter :in 1.0 :out 3.0)))
        (llm-pick-core--make-record "top"
                               :scores '((benchlm . 90))
                               :prices '((openrouter :in 1.0 :out 5.0)))
        (llm-pick-core--make-record "unpriced"
                               :scores '((benchlm . 95)))
        (llm-pick-core--make-record "unscored"
                               :prices '((openrouter :in 0.5 :out 4.0))))
  "Synthetic model records shared by the analysis tests.")

(defun llm-pick-analyze-test--names (models)
  "Return the canonical names of MODELS."
  (mapcar (lambda (model) (llm-pick-core--field model 'name)) models))

(ert-deftest llm-pick-analyze-test-frontier-is-pareto-optimal ()
  (let ((llm-pick-core-default-capability-source 'benchlm)
        (llm-pick-core-default-price-source 'openrouter))
    (ert-info ("Ordered by price, without dominated or incomplete models")
      (should (equal (llm-pick-analyze-test--names
                      (llm-pick-analyze--frontier llm-pick-analyze-test--models))
                     '("weak-cheap" "mid" "strong" "top"))))))

(ert-deftest llm-pick-analyze-test-dominance ()
  (let ((mid (nth 1 llm-pick-analyze-test--models))
        (dominated (nth 3 llm-pick-analyze-test--models))
        (llm-pick-core-default-capability-source 'benchlm)
        (llm-pick-core-default-price-source 'openrouter))
    (ert-info ("A cheaper and more capable model dominates")
      (should (llm-pick-analyze--dominates-p mid dominated))
      (should-not (llm-pick-analyze--dominates-p dominated mid)))
    (ert-info ("A model never dominates itself")
      (should-not (llm-pick-analyze--dominates-p mid mid)))))

(ert-deftest llm-pick-analyze-test-marginal-gain-per-step ()
  (let ((llm-pick-core-default-capability-source 'benchlm)
        (llm-pick-core-default-price-source 'openrouter))
    (let ((steps (llm-pick-analyze--marginal
                  (llm-pick-analyze--frontier llm-pick-analyze-test--models))))
      (ert-info ("The first step has no gain; the others are points per dollar")
        (should (equal (mapcar (lambda (step)
                                 (list (llm-pick-core--field (car step) 'name)
                                       (not (null (cdr step)))))
                               steps)
                       '(("weak-cheap" nil) ("mid" t) ("strong" t) ("top" t)))))
      (ert-info ("(75-60)/(1.0-0.5) = 30, (85-75)/(2.0-1.0) = 10, (90-85)/(5.0-2.0)")
        (should (= (cdr (nth 1 steps)) 30.0))
        (should (= (cdr (nth 2 steps)) 10.0))
        (should (= (cdr (nth 3 steps)) (/ 5 3.0)))))))

(ert-deftest llm-pick-analyze-test-ladder-buckets ()
  (let ((llm-pick-core-default-capability-source 'benchlm)
        (llm-pick-core-default-price-source 'openrouter))
    (ert-info ("Every price band reports its most capable model; (bound . name)")
      (should (equal (mapcar (lambda (bucket)
                               (cons (car bucket)
                                     (llm-pick-core--field (cdr bucket) 'name)))
                             (llm-pick-analyze--ladder llm-pick-analyze-test--models
                                               '(1.0 2.0 5.0 nil)))
                     '((1.0 . "weak-cheap") (2.0 . "mid")
                       (5.0 . "strong") (nil . "top")))))
    (ert-info ("Empty buckets are left out")
      (should (equal (mapcar #'car
                             (llm-pick-analyze--ladder llm-pick-analyze-test--models
                                               '(0.25 1.0 nil)))
                     '(1.0 nil))))))

(ert-deftest llm-pick-analyze-test-best-value ()
  (let ((llm-pick-core-default-capability-source 'benchlm)
        (llm-pick-core-default-price-source 'openrouter))
    (ert-info ("60/0.5 = 120 beats the capability per dollar of every other model")
      (should (equal (llm-pick-core--field
                      (llm-pick-analyze--best-value llm-pick-analyze-test--models)
                      'name)
                     "weak-cheap")))))

(ert-deftest llm-pick-analyze-test-next-upgrade ()
  (let ((mid (nth 1 llm-pick-analyze-test--models))
        (top (nth 4 llm-pick-analyze-test--models))
        (llm-pick-core-default-capability-source 'benchlm)
        (llm-pick-core-default-price-source 'openrouter))
    (ert-info ("The cheapest better model wins, not the most capable one")
      (should (equal (llm-pick-core--field
                      (llm-pick-analyze--next-upgrade llm-pick-analyze-test--models mid)
                      'name)
                     "strong")))
    (ert-info ("A model with no price cannot be an upgrade")
      (should (null (llm-pick-analyze--next-upgrade llm-pick-analyze-test--models top))))))

(ert-deftest llm-pick-analyze-test-analysis-does-not-reorder-its-input ()
  ;; The analysis sorts, and `cl-remove-if' hands back its argument when
  ;; it removes nothing, so the sort used to run on the caller's list.
  ;; A list shared with another test then reached it in a different
  ;; order, and that test failed for a reason that had nothing to do
  ;; with it.
  (let ((models (list (llm-pick-core--make-record "expensive"
                                             :scores '((benchlm . 90))
                                             :prices '((openrouter :in 1.0 :out 5.0)))
                      (llm-pick-core--make-record "cheap"
                                             :scores '((benchlm . 60))
                                             :prices '((openrouter :in 0.05 :out 0.5)))))
        (llm-pick-core-default-capability-source 'benchlm)
        (llm-pick-core-default-price-source 'openrouter))
    (ert-info ("The frontier still comes out ordered by price")
      (should (equal (llm-pick-analyze-test--names (llm-pick-analyze--frontier models))
                     '("cheap" "expensive"))))
    (ert-info ("The input list keeps the order the caller gave it")
      (should (equal (llm-pick-analyze-test--names models)
                     '("expensive" "cheap"))))
    (ert-info ("The other entry points leave the input alone as well")
      (llm-pick-analyze--best-value models)
      (should (equal (llm-pick-analyze-test--names models)
                     '("expensive" "cheap")))
      (llm-pick-analyze--marginal models)
      (should (equal (llm-pick-analyze-test--names models)
                     '("expensive" "cheap"))))))

(provide 'llm-pick-analyze-test)

;;; llm-pick-analyze-test.el ends here
