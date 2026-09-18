;;; llm-pick-core-test.el --- Tests for llm-pick-core -*- lexical-binding: t; -*-

;;; Commentary:

;;
;; Field access and predicate evaluation.  Every test binds the source
;; options it depends on, so the suite does not depend on the
;; configuration of the running Emacs.

;;; Code:

(require 'ert)
(require 'llm-pick-core)

(defconst llm-pick-core-test--record
  (llm-pick-core--make-record "claude-3-5-sonnet"
                         :display-name "Claude 3.5 Sonnet"
                         :scores '((benchlm . 88) (other . 70))
                         :prices '((openrouter :in 3.0 :out 15.0))
                         :providers '((openrouter . "anthropic/claude-3.5-sonnet")
                                      (anthropic . "claude-3-5-sonnet-20241022"))
                         :scope 'both)
  "Model record shared by the field access tests.")

(ert-deftest llm-pick-core-test-make-record-defaults ()
  (let ((record (llm-pick-core--make-record "gpt-4o")))
    (ert-info ("A bare record carries the canonical ID as display name")
      (should (equal (plist-get record :canonical) "gpt-4o"))
      (should (equal (plist-get record :display-name) "gpt-4o")))
    (ert-info ("A bare record has no data yet")
      (should (equal (mapcar (lambda (key) (plist-get record key))
                             '(:scores :prices :providers))
                     '(nil nil nil)))
      (should (eq (plist-get record :scope) 'unknown)))))

(ert-deftest llm-pick-core-test-make-record-merges-args ()
  (let ((record (llm-pick-core--make-record "gpt-4o"
                                       :display-name "GPT-4o"
                                       :scores '((benchlm . 92))
                                       :scope 'both)))
    (ert-info ("ARGS overrides the defaults and adds new fields")
      (should (equal (mapcar (lambda (key) (plist-get record key))
                             '(:canonical :display-name :scores :scope))
                     '("gpt-4o" "GPT-4o" ((benchlm . 92)) both))))))

(ert-deftest llm-pick-core-test-field-shorthands ()
  (let ((m llm-pick-core-test--record)
        (llm-pick-core-default-capability-source 'benchlm)
        (llm-pick-core-default-price-source 'openrouter))
    (ert-info ("`name' is the canonical ID")
      (should (equal (llm-pick-core--field m 'name) "claude-3-5-sonnet")))
    (ert-info ("`provider' is the first provider of the record")
      (should (eq (llm-pick-core--field m 'provider) 'openrouter)))
    (ert-info ("`scope' reports what the record can be used for")
      (should (eq (llm-pick-core--field m 'scope) 'both)))
    (ert-info ("`score' follows `llm-pick-core-default-capability-source'")
      (should (equal (llm-pick-core--field m 'score) 88)))
    (ert-info ("`or-in' and `or-out' follow `llm-pick-core-default-price-source'")
      (should (equal (llm-pick-core--field m 'or-in) 3.0))
      (should (equal (llm-pick-core--field m 'or-out) 15.0)))))

(ert-deftest llm-pick-core-test-field-explicit-source ()
  (let ((m llm-pick-core-test--record))
    (ert-info ("An explicit score source overrides the default")
      (should (equal (llm-pick-core--field m '(score other)) 70)))
    (ert-info ("An explicit price source and direction is honored")
      (should (equal (llm-pick-core--field m '(price openrouter in)) 3.0)))
    (ert-info ("An unknown source yields nil instead of an error")
      (should (null (llm-pick-core--field m '(score nope)))))))

(ert-deftest llm-pick-core-test-field-missing-data-returns-nil ()
  (let ((m (llm-pick-core--make-record "bare"))
        (llm-pick-core-default-price-source 'openrouter))
    (ert-info ("A record without scores or prices has nil fields")
      (should (equal (mapcar (lambda (field) (llm-pick-core--field m field))
                             '(score or-in or-out value provider))
                     '(nil nil nil nil nil))))))

(ert-deftest llm-pick-core-test-field-unknown-field-signals ()
  (ert-info ("A typo in a field name must not be answered with nil")
    (should-error (llm-pick-core--field llm-pick-core-test--record 'capability)
                  :type 'error)))

(ert-deftest llm-pick-core-test-field-secondary-price-and-gap ()
  (let ((m (llm-pick-core--make-record
            "m" :prices '((openrouter :in 3.0 :out 15.0)
                          (bedrock :in 3.0 :out 12.0))))
        (llm-pick-core-default-price-source 'openrouter)
        (llm-pick-core-secondary-price-source 'bedrock))
    (ert-info ("The second price source has its own shorthands")
      (should (equal (llm-pick-core--field m 'bm-out) 12.0))
      (should (equal (llm-pick-core--field m 'or-out) 15.0)))
    (ert-info ("The gap is the relative difference, on the cheaper baseline")
      (should (= (llm-pick-core--field m 'gap) 0.25)))
    (ert-info ("With no second price source registered there is no gap")
      (let ((llm-pick-core-secondary-price-source nil))
        (should (null (llm-pick-core--field m 'bm-out)))
        (should (null (llm-pick-core--field m 'gap)))))
    (ert-info ("A model without the second price has no gap either")
      (should (null (llm-pick-core--field (llm-pick-core--make-record "bare") 'gap))))))

(ert-deftest llm-pick-core-test-field-keeps-categories-apart ()
  (let ((m (llm-pick-core--make-record
            "m" :categories '("coding" "math")
            :scores '(((benchlm . "coding") . 88)
                      ((benchlm . "math") . 80))))
        (llm-pick-core-default-capability-source 'benchlm))
    (ert-info ("A category column reads its own score")
      (should (equal (llm-pick-core--field m '(score benchlm "coding")) 88))
      (should (equal (llm-pick-core--field m '(score benchlm "math")) 80)))
    (ert-info ("The `score' shorthand answers with the first category")
      (should (equal (llm-pick-core--field m 'score) 88)))
    (ert-info ("An unknown category yields nil instead of an error")
      (should (null (llm-pick-core--field m '(score benchlm "nope")))))))

(ert-deftest llm-pick-core-test-match-p-reads-a-symbol-field-as-a-name ()
  (let ((m (llm-pick-core--make-record "a"
                                  :providers '((anthropic . "claude")))))
    (ert-info ("`~' matches the name of a symbol field")
      (should (llm-pick-core--match-p m '(~ provider "anthropic\\|openai")))
      (should-not (llm-pick-core--match-p m '(~ provider "google"))))
    (ert-info ("`=' compares a symbol field with the text of a string")
      (should (llm-pick-core--match-p m '(= provider "anthropic")))
      (should-not (llm-pick-core--match-p m '(= provider "google"))))
    (ert-info ("A field the record does not carry still never matches")
      (should-not (llm-pick-core--match-p m '(~ scope "both"))))))

(ert-deftest llm-pick-core-test-match-p-numeric ()
  (let ((m (llm-pick-core--make-record "a" :scores '((benchlm . 88))))
        (llm-pick-core-default-capability-source 'benchlm))
    (ert-info ("88 satisfies > 75 and >= 88")
      (should (llm-pick-core--match-p m '(> score 75)))
      (should (llm-pick-core--match-p m '(>= score 88))))
    (ert-info ("88 does not satisfy > 90 nor <= 87")
      (should-not (llm-pick-core--match-p m '(> score 90)))
      (should-not (llm-pick-core--match-p m '(<= score 87))))))

(ert-deftest llm-pick-core-test-match-p-missing-field ()
  (let ((m (llm-pick-core--make-record "bare"))
        (llm-pick-core-default-capability-source 'benchlm))
    (ert-info ("A missing field fails the comparison instead of signaling")
      (should-not (llm-pick-core--match-p m '(> score 0)))
      (should-not (llm-pick-core--match-p m '(= score 0))))))

(ert-deftest llm-pick-core-test-match-p-string-and-membership ()
  (let ((m (llm-pick-core--make-record "claude-3-5-sonnet" :scope 'both)))
    (ert-info ("The regexp predicate matches on the string field")
      (should (llm-pick-core--match-p m '(~ name "\\`claude")))
      (should-not (llm-pick-core--match-p m '(~ name "\\`gpt"))))
    (ert-info ("The membership predicate works on symbol fields")
      (should (llm-pick-core--match-p m '(in scope (both capability-only))))
      (should-not (llm-pick-core--match-p m '(in scope (price-only)))))))

(ert-deftest llm-pick-core-test-filter ()
  (let ((models (list (llm-pick-core--make-record "cheap-strong"
                                             :scores '((benchlm . 80))
                                             :prices '((openrouter :out 1.0)))
                      (llm-pick-core--make-record "cheap-weak"
                                             :scores '((benchlm . 60))
                                             :prices '((openrouter :out 1.0)))
                      (llm-pick-core--make-record "pricey-strong"
                                             :scores '((benchlm . 90))
                                             :prices '((openrouter :out 9.0)))))
        (llm-pick-core-default-capability-source 'benchlm)
        (llm-pick-core-default-price-source 'openrouter))
    (ert-info ("Every predicate must hold at once")
      (should (equal (mapcar (lambda (m) (llm-pick-core--field m 'name))
                             (llm-pick-core--filter models '((> score 75) (< or-out 5))))
                     '("cheap-strong"))))
    (ert-info ("A nil WHERE keeps every model")
      (should (equal (llm-pick-core--filter models nil) models)))))

(ert-deftest llm-pick-core-test-name-match-forgives-spelling ()
  (let ((m (llm-pick-core--make-record "deepseek-v3.2"
                                       :display-name "DeepSeek V3.2")))
    (ert-info ("A name with a space and different case still matches")
      (should (llm-pick-core--match-p m '(~ name "DeepSeek 3")))
      (should (llm-pick-core--match-p m '(~ name "deepseek 3"))))
    (ert-info ("A regexp matching the canonical ID keeps working")
      (should (llm-pick-core--match-p m '(~ name "deepseek-v3")))
      (should-not (llm-pick-core--match-p m '(~ name "\\`gpt"))))
    (ert-info ("A regexp with no alphanumeric word matches nothing")
      (should-not (llm-pick-core--match-p m '(~ name "  "))))))

(ert-deftest llm-pick-core-test-vendor-and-family ()
  (ert-info ("Vendor is derived from the canonical ID")
    (should (equal (llm-pick-core--field (llm-pick-core--make-record "claude-3-5-sonnet") 'vendor)
                   "Anthropic"))
    (should (equal (llm-pick-core--field (llm-pick-core--make-record "deepseek-v3.2") 'vendor)
                   "DeepSeek")))
  (ert-info ("Family is derived from the canonical ID")
    (should (equal (llm-pick-core--field (llm-pick-core--make-record "claude-3-5-sonnet") 'family)
                   "claude"))
    (should (equal (llm-pick-core--field (llm-pick-core--make-record "deepseek-v3.2") 'family)
                   "deepseek")))
  (ert-info ("Unknown IDs have no vendor or family")
    (should-not (llm-pick-core--field (llm-pick-core--make-record "mystery-1") 'vendor))
    (should-not (llm-pick-core--field (llm-pick-core--make-record "mystery-1") 'family))))

(provide 'llm-pick-core-test)

;;; llm-pick-core-test.el ends here
