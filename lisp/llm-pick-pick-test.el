;;; llm-pick-pick-test.el --- Tests for llm-pick-pick -*- lexical-binding: t; -*-

;;; Commentary:

;;
;; Choosing and resolving are pure functions over records, so the tests
;; use synthetic records whose expectations can be checked by hand, and
;; they bind the source options they depend on.

;;; Code:

(require 'ert)
(require 'llm-pick-pick)

(defconst llm-pick-pick-test--models
  (list (llm-pick-core--make-record "claude"
                               :scores '((benchlm . 88))
                               :prices '((openrouter :in 3.0 :out 15.0))
                               :providers '((anthropic . "claude-3-5-sonnet-20241022")
                                            (openrouter . "anthropic/claude-3.5-sonnet")))
        (llm-pick-core--make-record "flash"
                               :scores '((benchlm . 82))
                               :prices '((openrouter :in 0.075 :out 0.3))
                               :providers '((openrouter . "google/gemini-flash-1.5")))
        (llm-pick-core--make-record "mini"
                               :scores '((benchlm . 78))
                               :prices '((openrouter :in 0.15 :out 0.6))
                               :providers '((openai . "gpt-4o-mini")))
        (llm-pick-core--make-record "unpriced"
                               :scores '((benchlm . 99))
                               :providers '((openai . "gpt-5")))
        (llm-pick-core--make-record "unscored"
                               :prices '((openrouter :in 0.35 :out 0.4))
                               :providers '((openrouter . "qwen/qwen-2.5-72b"))))
  "Records shared by the pick tests.")

(defmacro llm-pick-pick-test--with-sources (&rest body)
  "Run BODY with the source options the suite needs bound."
  (declare (indent 0))
  `(let ((llm-pick-core-default-capability-source 'benchlm)
         (llm-pick-core-default-price-source 'openrouter))
     ,@body))

(defun llm-pick-pick-test--name (model)
  "Return the canonical name of MODEL."
  (llm-pick-core--field model 'name))

(ert-deftest llm-pick-pick-test-budget-takes-the-most-capable ()
  (llm-pick-pick-test--with-sources
    (ert-info ("Under $1/M out the strongest comparable model is flash")
      (should (equal (llm-pick-pick-test--name
                      (llm-pick-pick--choose llm-pick-pick-test--models :budget 1.0))
                     "flash")))))

(ert-deftest llm-pick-pick-test-target-score-takes-the-cheapest ()
  (llm-pick-pick-test--with-sources
    (ert-info ("80 points is reached by claude at $15 and flash at $0.3")
      (should (equal (llm-pick-pick-test--name
                      (llm-pick-pick--choose llm-pick-pick-test--models
                                        :target-score 80))
                     "flash")))
    (ert-info ("Adding a budget flips the ranking back to capability first")
      (should (equal (llm-pick-pick-test--name
                      (llm-pick-pick--choose llm-pick-pick-test--models
                                        :target-score 80 :budget 20.0))
                     "claude")))))

(ert-deftest llm-pick-pick-test-both-criteria-must-hold ()
  (llm-pick-pick-test--with-sources
    (ert-info ("A budget wide enough for claude leaves claude as the strongest")
      (should (equal (llm-pick-pick-test--name
                      (llm-pick-pick--choose llm-pick-pick-test--models
                                        :budget 20.0 :target-score 85))
                     "claude")))))

(ert-deftest llm-pick-pick-test-available-on-narrows-the-choice ()
  (llm-pick-pick-test--with-sources
    (ert-info ("Only mini is sold by openai")
      (should (equal (llm-pick-pick-test--name
                      (llm-pick-pick--choose llm-pick-pick-test--models
                                        :budget 1.0 :available-on '(openai)))
                     "mini")))))

(ert-deftest llm-pick-pick-test-incomplete-models-never-win ()
  (llm-pick-pick-test--with-sources
    (ert-info ("The 99 point model has no price and cannot be shown to be affordable")
      (should (equal (llm-pick-pick-test--name
                      (llm-pick-pick--choose llm-pick-pick-test--models
                                        :budget 100.0))
                     "claude")))
    (ert-info ("A model without a score cannot be shown to be the most capable")
      (should-not (equal (llm-pick-pick-test--name
                          (llm-pick-pick--choose llm-pick-pick-test--models
                                            :budget 100.0))
                         "unscored")))))

(ert-deftest llm-pick-pick-test-nothing-qualifies-is-an-error ()
  (llm-pick-pick-test--with-sources
    (ert-info ("No affordable model is an error, not nil")
      (should-error (llm-pick-pick--choose llm-pick-pick-test--models :budget 0.01)
                    :type 'llm-pick-error))))

(ert-deftest llm-pick-pick-test-resolve ()
  (llm-pick-pick-test--with-sources
    (ert-info ("A known canonical ID resolves to its provider ID")
      (should (equal (llm-pick-pick--resolve llm-pick-pick-test--models
                                        "claude" 'anthropic)
                     '(anthropic . "claude-3-5-sonnet-20241022"))))
    (ert-info ("An unknown canonical ID is an error")
      (should-error (llm-pick-pick--resolve llm-pick-pick-test--models
                                       "nope" 'openrouter)
                    :type 'llm-pick-error))
    (ert-info ("A provider that does not carry the model is an error")
      (should-error (llm-pick-pick--resolve llm-pick-pick-test--models
                                       "flash" 'anthropic)
                    :type 'llm-pick-error))))

(ert-deftest llm-pick-pick-test-resolve-with-fallback ()
  (llm-pick-pick-test--with-sources
    (ert-info ("The first provider that names the model wins")
      (should (equal (llm-pick-pick--resolve-with-fallback
                      llm-pick-pick-test--models "claude"
                      '(bedrock anthropic openrouter))
                     '(anthropic . "claude-3-5-sonnet-20241022"))))
    (ert-info ("A single available provider is enough")
      (should (equal (llm-pick-pick--resolve-with-fallback
                      llm-pick-pick-test--models "mini" '(openai))
                     '(openai . "gpt-4o-mini"))))
    (ert-info ("No provider naming the model is an error")
      (should-error (llm-pick-pick--resolve-with-fallback
                     llm-pick-pick-test--models "mini" '(bedrock openrouter))
                    :type 'llm-pick-error))))

(provide 'llm-pick-pick-test)

;;; llm-pick-pick-test.el ends here
