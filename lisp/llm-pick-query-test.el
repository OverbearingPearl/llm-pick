;;; llm-pick-query-test.el --- Tests for llm-pick-query-run-query -*- lexical-binding: t; -*-

;;; Commentary:

;;
;; The tests use synthetic records so that every expectation can be
;; checked by hand, and they bind the source options they depend on.

;;; Code:

(require 'ert)
(require 'llm-pick-query-run)

(defconst llm-pick-query-test--models
  (list (llm-pick-core--make-record "claude"
                               :scores '((benchlm . 88))
                               :prices '((openrouter :in 3.0 :out 15.0))
                               :providers '((anthropic . "claude-3-5-sonnet-20241022")
                                            (openrouter . "anthropic/claude-3.5-sonnet"))
                               :scope 'both)
        (llm-pick-core--make-record "flash"
                               :scores '((benchlm . 82))
                               :prices '((openrouter :in 0.075 :out 0.3))
                               :providers '((openrouter . "google/gemini-flash-1.5"))
                               :scope 'both)
        (llm-pick-core--make-record "mini"
                               :scores '((benchlm . 78))
                               :prices '((openrouter :in 0.15 :out 0.6))
                               :providers '((openai . "gpt-4o-mini"))
                               :scope 'both)
        (llm-pick-core--make-record "orphan"
                               :scores '((benchlm . 70))
                               :scope 'capability-only)
        (llm-pick-core--make-record "unscored"
                               :prices '((openrouter :in 0.35 :out 0.4))
                               :scope 'price-only))
  "Records shared by the query tests.")

(defun llm-pick-query-test--names (models)
  "Return the canonical names of MODELS."
  (mapcar (lambda (model) (llm-pick-core--field model 'name)) models))

(ert-deftest llm-pick-query-test-budget-drops-expensive-and-unpriced ()
  (let ((llm-pick-core-default-capability-source 'benchlm)
        (llm-pick-core-default-price-source 'openrouter))
    (ert-info ("Budget is the output price; a model without one cannot be shown to be affordable")
      (should (equal (llm-pick-query-test--names
                      (llm-pick-query-run-query llm-pick-query-test--models :budget 3.0))
                     '("flash" "mini" "unscored"))))))

(ert-deftest llm-pick-query-test-target-score-drops-weak-and-unscored ()
  (let ((llm-pick-core-default-capability-source 'benchlm)
        (llm-pick-core-default-price-source 'openrouter))
    (ert-info ("Only the models reaching the score survive")
      (should (equal (llm-pick-query-test--names
                      (llm-pick-query-run-query llm-pick-query-test--models :target-score 80))
                     '("claude" "flash"))))))

(ert-deftest llm-pick-query-test-scope-selects-what-a-model-can-do ()
  (let ((llm-pick-core-default-capability-source 'benchlm)
        (llm-pick-core-default-price-source 'openrouter))
    (ert-info ("A single scope selects one kind of record")
      (should (equal (llm-pick-query-test--names
                      (llm-pick-query-run-query llm-pick-query-test--models
                                      :scope 'price-only))
                     '("unscored"))))
    (ert-info ("A list of scopes selects several")
      (should (equal (llm-pick-query-test--names
                      (llm-pick-query-run-query llm-pick-query-test--models
                                      :scope '(capability-only price-only)))
                     '("orphan" "unscored"))))))

(ert-deftest llm-pick-query-test-available-on-filters-by-provider ()
  (let ((llm-pick-core-default-capability-source 'benchlm)
        (llm-pick-core-default-price-source 'openrouter))
    (ert-info ("A model is kept when it carries an ID for one of the providers")
      (should (equal (llm-pick-query-test--names
                      (llm-pick-query-run-query llm-pick-query-test--models
                                      :available-on '(anthropic)))
                     '("claude"))))
    (ert-info ("Several providers widen the selection")
      (should (equal (llm-pick-query-test--names
                      (llm-pick-query-run-query llm-pick-query-test--models
                                      :available-on '(openai openrouter)))
                     '("claude" "flash" "mini"))))))

(ert-deftest llm-pick-query-test-order-descending-and-top ()
  (let ((llm-pick-core-default-capability-source 'benchlm)
        (llm-pick-core-default-price-source 'openrouter))
    (ert-info ("Ascending price first, a model without a price last")
      (should (equal (llm-pick-query-test--names
                      (llm-pick-query-run-query llm-pick-query-test--models :order 'or-out))
                     '("flash" "unscored" "mini" "claude" "orphan"))))
    (ert-info ("Descending still keeps a model without the field last")
      (should (equal (llm-pick-query-test--names
                      (llm-pick-query-run-query llm-pick-query-test--models
                                      :order 'or-out :descending t))
                     '("claude" "mini" "unscored" "flash" "orphan"))))
    (ert-info (":top keeps the first models of the sorted order")
      (should (equal (llm-pick-query-test--names
                      (llm-pick-query-run-query llm-pick-query-test--models
                                      :order 'score :descending t :top 2))
                     '("claude" "flash"))))
    (ert-info ("A :top larger than the list is harmless")
      (should (= (length (llm-pick-query-run-query llm-pick-query-test--models :top 99))
                 (length llm-pick-query-test--models))))))

(ert-deftest llm-pick-query-test-where-and-combined-criteria ()
  (let ((llm-pick-core-default-capability-source 'benchlm)
        (llm-pick-core-default-price-source 'openrouter))
    (ert-info (":where takes the predicates of `llm-pick-core--match-p'")
      (should (equal (llm-pick-query-test--names
                      (llm-pick-query-run-query llm-pick-query-test--models
                                      :where '((> score 75) (< or-out 1))))
                     '("flash" "mini"))))
    (ert-info ("A provider filter that only one model passes drops the rest")
      (should (equal (llm-pick-query-test--names
                      (llm-pick-query-run-query llm-pick-query-test--models
                                      :where '((>= score 78))
                                      :available-on '(openrouter)
                                      :budget 1.0
                                      :order 'score
                                      :descending t))
                     '("flash"))))
    (ert-info ("The criteria combine")
      (should (equal (llm-pick-query-test--names
                      (llm-pick-query-run-query llm-pick-query-test--models
                                      :where '((> score 75))
                                      ;; Both providers are listed, because
                                      ;; mini is only sold by openai; with
                                      ;; `openrouter' alone it would be
                                      ;; dropped before the budget applies.
                                      :available-on '(openai openrouter)
                                      :budget 1.0
                                      :order 'score
                                      :descending t))
                     '("flash" "mini"))))))

(ert-deftest llm-pick-query-test-unknown-argument-signals ()
  (ert-info ("A typo in an argument must not be answered with the unfiltered list")
    (should-error (llm-pick-query-run-query llm-pick-query-test--models :budjet 3.0)
                  :type 'llm-pick-error)))

(provide 'llm-pick-query-test)

;;; llm-pick-query-test.el ends here
