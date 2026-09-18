;;; llm-pick-source-test.el --- Tests for llm-pick-source -*- lexical-binding: t; -*-

;;; Commentary:

;;
;; The tests never open a socket: the service loaders read the inline
;; JSON below, shaped like the answers the services document, and never
;; depend on the configuration of the running Emacs.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'llm-pick-source)

(defconst llm-pick-source-test--leaderboard
  "{\"lastUpdated\": \"September 2, 2026\",
    \"models\": [
      {\"rank\": 1, \"model\": \"Claude Fable 5.1\", \"creator\": \"Anthropic\",
       \"overallScore\": 82.95,
       \"categoryScores\": {\"coding\": 79.55, \"math\": null}},
      {\"rank\": 2, \"model\": \"GPT-5\", \"creator\": \"OpenAI\",
       \"categoryScores\": {\"coding\": 91.0, \"math\": 88.0}}]}"
  "A leaderboard answer, shaped like the one BenchLM documents.")

(defconst llm-pick-source-test--model-list
  "{\"data\": [
     {\"id\": \"openai/gpt-4o\", \"name\": \"OpenAI: GPT-4o\",
      \"pricing\": {\"prompt\": \"0.000005\", \"completion\": \"0.000015\"}},
     {\"id\": \"meta-llama/llama-3.1-8b-instruct\", \"name\": \"Llama 3.1 8B\",
      \"pricing\": {\"prompt\": \"0.00000005\", \"completion\": \"0.0000001\"}}]}"
  "A model list answer, shaped like the one OpenRouter documents.
The prices are per token, which is how the service quotes them.")

(defconst llm-pick-source-test--collect-leaderboard
  "{\"models\": [
     {\"model\": \"Claude 3.5 Sonnet\", \"creator\": \"Anthropic\",
      \"categoryScores\": {\"coding\": 88, \"math\": 80}},
     {\"model\": \"GPT-4o\", \"creator\": \"OpenAI\",
      \"provider_ids\": {\"openai\": \"gpt-4o\"},
      \"categoryScores\": {\"coding\": 92, \"math\": 90}},
     {\"model\": \"GPT-4o mini\", \"creator\": \"OpenAI\",
      \"categoryScores\": {\"coding\": 78, \"math\": 70}},
     {\"model\": \"Gemini 1.5 Flash\", \"creator\": \"Google\",
      \"categoryScores\": {\"coding\": 82, \"math\": 84}},
     {\"model\": \"llama-3.1-8b-instruct\", \"creator\": \"meta-llama\",
      \"categoryScores\": {\"coding\": 65, \"math\": 60}},
     {\"model\": \"Orphan Model\", \"creator\": \"Acme\",
      \"categoryScores\": {\"coding\": 70, \"math\": 70}}]}"
  "A leaderboard answer for the collect tests.
It carries the same six models the collect expectations name, with
the coding and math scores they expect.  GPT-4o carries its OpenAI
provider ID, so the provider merge has something to work with.  The
Llama entry names the instruct slug with the meta-llama creator, so
its ID normalizes to the same canonical llama-3-1-8b-instruct as the
OpenRouter entry.")

(defconst llm-pick-source-test--collect-model-list
  "{\"data\": [
     {\"id\": \"anthropic/claude-3.5-sonnet\", \"name\": \"Claude 3.5 Sonnet\",
      \"pricing\": {\"prompt\": \"0.000003\", \"completion\": \"0.000015\"}},
     {\"id\": \"openai/gpt-4o\", \"name\": \"OpenAI: GPT-4o\",
      \"pricing\": {\"prompt\": \"0.000005\", \"completion\": \"0.000015\"}},
     {\"id\": \"openai/gpt-4o-mini\", \"name\": \"OpenAI: GPT-4o mini\",
      \"pricing\": {\"prompt\": \"0.00000015\", \"completion\": \"0.0000006\"}},
     {\"id\": \"google/gemini-flash-1.5\", \"name\": \"Gemini Flash 1.5\",
      \"pricing\": {\"prompt\": \"0.000000075\", \"completion\": \"0.0000003\"}},
     {\"id\": \"meta-llama/llama-3.1-8b-instruct\", \"name\": \"Llama 3.1 8B\",
      \"pricing\": {\"prompt\": \"0.00000005\", \"completion\": \"0.0000001\"}},
     {\"id\": \"qwen/qwen-2.5-72b\", \"name\": \"Qwen 2.5 72B\",
      \"pricing\": {\"prompt\": \"0.00000035\", \"completion\": \"0.0000004\"}}]}"
  "A model list for the collect tests.
The prices are per token and scale to the per-million values the
collect expectations name.")

(defmacro llm-pick-source-test--with-stubs (&rest body)
  "Run BODY with the services answered by the inline JSON above.
No socket is opened, and the in-process leaderboard cache starts
empty so nothing leaks between tests."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'llm-pick-fetch-get-json)
              (lambda (url &rest _)
                (cond
                 ((string-prefix-p "https://benchlm.ai" url)
                  (llm-pick-core--parse-json
                   llm-pick-source-test--collect-leaderboard))
                 ((string-prefix-p "https://openrouter.ai/api/v1/models" url)
                  (llm-pick-core--parse-json
                   llm-pick-source-test--collect-model-list))
                 ((string-prefix-p "https://openrouter.ai/api/v1/benchmarks" url)
                  (llm-pick-core--parse-json "{\"data\": []}"))
                 (t (error "Unexpected URL in test: %s" url)))))
             (llm-pick-source--benchlm-cache nil))
     ,@body))

(defun llm-pick-source-test--score-of (name records)
  "Return the score of the record named NAME among RECORDS."
  (llm-pick-core--field (car (cl-remove-if-not
                             (lambda (record)
                               (equal (llm-pick-core--field record 'name) name))
                             records))
                        'score))

(ert-deftest llm-pick-source-test-register-appends-and-replaces ()
  (let ((llm-pick-source-sources llm-pick-source-sources))
    (llm-pick-source-register 'test-source :kind 'capability
                              :description "first"
                              :loader #'ignore)
    (ert-info ("A new source is appended in registration order")
      (should (equal (mapcar #'car llm-pick-source-sources)
                     '(benchlm openrouter test-source))))
    (llm-pick-source-register 'test-source :kind 'both
                              :description "second"
                              :loader #'ignore)
    (ert-info ("Re-registering keeps the position and replaces the descriptor")
      (should (equal (mapcar #'car llm-pick-source-sources)
                     '(benchlm openrouter test-source)))
      (should (equal (plist-get (cdr (assq 'test-source llm-pick-source-sources))
                                :description)
                     "second")))))

(ert-deftest llm-pick-source-test-collect-merges-sources ()
  (let ((llm-pick-core-default-capability-source 'benchlm)
        (llm-pick-core-default-price-source 'openrouter)
        (llm-pick-align-match-threshold 0.85)
        (llm-pick-align-match-ambiguity-gap 0.05)
        (llm-pick-align-on-unmatched 'standalone))
    (llm-pick-source-test--with-stubs
      (let ((records (llm-pick-source--collect :sources '(benchlm openrouter)
                                               :category "coding")))
        (ert-info ("One record per canonical ID; (name score or-out scope)")
          (should (equal (mapcar (lambda (record)
                                   (list (llm-pick-core--field record 'name)
                                         (llm-pick-core--field record 'score)
                                         (llm-pick-core--field record 'or-out)
                                         (llm-pick-core--field record 'scope)))
                                 records)
                         '(("acme-orphan-model" 70 nil capability-only)
                           ("claude-3-5-sonnet" 88 15.0 both)
                           ("gemini-1-5-flash" 82 0.3 both)
                           ("gpt-4o" 92 15.0 both)
                           ("gpt-4o-mini" 78 0.6 both)
                           ("llama-3-1-8b" 65 0.1 both)
                           ("qwen-2-5-72b" nil 0.4 price-only)))))
        (ert-info ("Provider IDs are merged across sources; (provider . id)")
          (should (equal (plist-get (car (cl-remove-if-not
                                          (lambda (record)
                                            (equal (llm-pick-core--field record 'name)
                                                   "gpt-4o"))
                                          records))
                                    :providers)
                         '((openrouter . "openai/gpt-4o")))))))))

(ert-deftest llm-pick-source-test-collect-category-selects-score ()
  (let ((llm-pick-core-default-capability-source 'benchlm)
        (llm-pick-core-default-price-source 'openrouter))
    (llm-pick-source-test--with-stubs
      (let ((coding (llm-pick-source-test--score-of
                     "claude-3-5-sonnet"
                     (llm-pick-source--collect :sources '(benchlm) :category "coding")))
            (math (llm-pick-source-test--score-of
                   "claude-3-5-sonnet"
                   (llm-pick-source--collect :sources '(benchlm) :category "math")))
            (best (llm-pick-source-test--score-of
                   "claude-3-5-sonnet"
                   (llm-pick-source--collect :sources '(benchlm)))))
        (ert-info ("Each category selects its own score; nil selects the best one")
          (should (equal (list coding math best) '(88 80 88))))))))

(ert-deftest llm-pick-source-test-collect-without-capability-source ()
  (let ((llm-pick-core-default-capability-source 'benchlm)
        (llm-pick-core-default-price-source 'openrouter)
        (llm-pick-align-on-unmatched 'standalone))
    (llm-pick-source-test--with-stubs
      (let ((records (llm-pick-source--collect :sources '(openrouter))))
        (ert-info ("Without a capability source every record is price-only")
          (should (equal (delete-dups (mapcar (lambda (record)
                                                (llm-pick-core--field record 'scope))
                                              records))
                         '(price-only))))))))

(ert-deftest llm-pick-source-test-collect-two-categories-keys-the-scores ()
  (let ((llm-pick-core-default-capability-source 'benchlm)
        (llm-pick-core-default-price-source 'openrouter)
        (llm-pick-align-match-threshold 0.85)
        (llm-pick-align-match-ambiguity-gap 0.05)
        (llm-pick-align-on-unmatched 'standalone))
    (llm-pick-source-test--with-stubs
      (let* ((records (llm-pick-source--collect :sources '(benchlm)
                                                :category '("coding" "math")))
             (claude (car (cl-remove-if-not
                           (lambda (record)
                             (equal (llm-pick-core--field record 'name)
                                    "claude-3-5-sonnet"))
                           records))))
        (ert-info ("Each category is stored under a key of its own")
          (should (equal (plist-get claude :scores)
                         '(((benchlm . "coding") . 88)
                           ((benchlm . "math") . 80)))))
        (ert-info ("Both category columns read their own score")
          (should (equal (llm-pick-core--field claude '(score benchlm "coding")) 88))
          (should (equal (llm-pick-core--field claude '(score benchlm "math")) 80)))
        (ert-info ("The `score' shorthand still answers with the first category")
          (should (equal (llm-pick-core--field claude 'score) 88)))))))

(ert-deftest llm-pick-source-test-collect-bad-category-signals ()
  (ert-info ("A category that is not a name is a loud error")
    (should-error (llm-pick-source--collect :sources '(benchlm) :category 7)
                  :type 'llm-pick-error)))

(ert-deftest llm-pick-source-test-benchlm-url-carries-the-category ()
  (ert-info ("The category is a query parameter, the limit was removed")
    (should (equal (llm-pick-source--benchlm-url "coding")
                   "https://benchlm.ai/api/data/leaderboard?category=coding"))
    (should (equal (llm-pick-source--benchlm-url nil)
                   "https://benchlm.ai/api/data/leaderboard"))))

(ert-deftest llm-pick-source-test-benchlm-loader-reads-the-leaderboard ()
  (cl-letf (((symbol-function 'llm-pick-fetch-get-json)
             (lambda (&rest _)
  (llm-pick-core--parse-json llm-pick-source-test--leaderboard)))
            (llm-pick-source--benchlm-cache nil))
    (ert-info ("Creator and name make the ID; (id score category)")
      (should (equal (mapcar (lambda (entry)
                               (list (plist-get entry :id)
                                     (plist-get entry :score)
                                     (plist-get entry :category)))
                             (llm-pick-source--benchlm-loader '(:category "coding")))
                     '(("Anthropic/Claude Fable 5.1" 79.55 "coding")
                       ("OpenAI/GPT-5" 91.0 "coding")))))
    (ert-info ("A nil category takes the best score the model has")
      (should (equal (mapcar (lambda (entry)
                               (plist-get entry :score))
                             (llm-pick-source--benchlm-loader '(:category nil)))
                     '(79.55 91.0))))))

(ert-deftest llm-pick-source-test-openrouter-loader-converts-per-token-prices ()
  (cl-letf (((symbol-function 'llm-pick-fetch-get-json)
             (lambda (&rest _)
  (llm-pick-core--parse-json llm-pick-source-test--model-list))))
    (let ((entries (llm-pick-source--openrouter-loader nil)))
      (ert-info ("Every entry names the channel that sells it")
        (should (equal (mapcar (lambda (entry)
                                 (list (plist-get entry :id)
                                       (plist-get entry :providers)))
                               entries)
                       '(("openai/gpt-4o" ((openrouter . "openai/gpt-4o")))
                         ("meta-llama/llama-3.1-8b-instruct"
                          ((openrouter . "meta-llama/llama-3.1-8b-instruct")))))))
      (ert-info ("A per token quote comes out per million tokens")
        (should (= (plist-get (plist-get (car entries) :prices) :in) 5.0))
        (should (= (plist-get (plist-get (car entries) :prices) :out) 15.0))
        (should (= (plist-get (plist-get (cadr entries) :prices) :out) 0.1))))))

(ert-deftest llm-pick-source-test-collect-unknown-source-signals ()
  (ert-info ("A typo in a source name must not be answered with an empty list")
    (should-error (llm-pick-source--collect :sources '(nope))
                  :type 'llm-pick-error)))

(provide 'llm-pick-source-test)

;;; llm-pick-source-test.el ends here
