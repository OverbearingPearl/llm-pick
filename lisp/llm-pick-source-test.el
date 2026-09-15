;;; llm-pick-source-test.el --- Tests for llm-pick-source -*- lexical-binding: t; -*-

;;; Commentary:

;;
;; The tests run against the offline snapshots in a temporary directory the tests write, so they
;; never touch the network and never depend on the user configuration of
;; the running Emacs.

;;; Code:

(require 'ert)
(require 'llm-pick-source)

(defconst llm-pick-source-test--fixture-directory
  (let ((directory (make-temp-file "llm-pick-source-test-" t)))
    (dolist (snapshot
             '(("benchlm-sample.json" .
                "{
  \"note\": \"Illustrative sample data; not real benchmark results.\",
  \"models\": [
    {
      \"id\": \"claude-3.5-sonnet\",
      \"name\": \"Claude 3.5 Sonnet\",
      \"provider_ids\": { \"anthropic\": \"claude-3-5-sonnet-20241022\" },
      \"scores\": { \"coding\": 88, \"math\": 80 }
    },
    {
      \"id\": \"gpt-4o\",
      \"name\": \"GPT-4o\",
      \"provider_ids\": { \"openai\": \"gpt-4o\" },
      \"scores\": { \"coding\": 92, \"math\": 90 }
    },
    {
      \"id\": \"gpt-4o-mini\",
      \"name\": \"GPT-4o mini\",
      \"provider_ids\": { \"openai\": \"gpt-4o-mini\" },
      \"scores\": { \"coding\": 78, \"math\": 70 }
    },
    {
      \"id\": \"gemini-1.5-flash\",
      \"name\": \"Gemini 1.5 Flash\",
      \"scores\": { \"coding\": 82, \"math\": 84 }
    },
    {
      \"id\": \"llama-3.1-8b\",
      \"name\": \"Llama 3.1 8B\",
      \"scores\": { \"coding\": 65, \"math\": 60 }
    },
    {
      \"id\": \"orphan-model\",
      \"name\": \"Orphan Model\",
      \"scores\": { \"coding\": 70, \"math\": 70 }
    }
  ]
}")
               ("openrouter-sample.json" .
                "{
  \"note\": \"Illustrative sample prices; not real quotes.\",
  \"models\": [
    {
      \"id\": \"anthropic/claude-3.5-sonnet\",
      \"name\": \"Anthropic: Claude 3.5 Sonnet\",
      \"provider_ids\": { \"openrouter\": \"anthropic/claude-3.5-sonnet\" },
      \"pricing\": { \"prompt\": 3.0, \"completion\": 15.0 }
    },
    {
      \"id\": \"openai/gpt-4o\",
      \"name\": \"OpenAI: GPT-4o\",
      \"provider_ids\": {
        \"openrouter\": \"openai/gpt-4o\", \"openai\": \"gpt-4o\" },
      \"pricing\": { \"prompt\": 5.0, \"completion\": 15.0 }
    },
    {
      \"id\": \"openai/gpt-4o-mini\",
      \"name\": \"OpenAI: GPT-4o mini\",
      \"provider_ids\": { \"openrouter\": \"openai/gpt-4o-mini\" },
      \"pricing\": { \"prompt\": 0.15, \"completion\": 0.6 }
    },
    {
      \"id\": \"google/gemini-flash-1.5\",
      \"name\": \"Google: Gemini Flash 1.5\",
      \"provider_ids\": { \"openrouter\": \"google/gemini-flash-1.5\" },
      \"pricing\": { \"prompt\": 0.075, \"completion\": 0.3 }
    },
    {
      \"id\": \"meta-llama/llama-3.1-8b-instruct\",
      \"name\": \"Meta: Llama 3.1 8B Instruct\",
      \"provider_ids\": {
        \"openrouter\": \"meta-llama/llama-3.1-8b-instruct\" },
      \"pricing\": { \"prompt\": 0.05, \"completion\": 0.1 }
    },
    {
      \"id\": \"qwen/qwen-2.5-72b\",
      \"name\": \"Qwen 2.5 72B\",
      \"provider_ids\": { \"openrouter\": \"qwen/qwen-2.5-72b\" },
      \"pricing\": { \"prompt\": 0.35, \"completion\": 0.4 }
    }
  ]
}")))
      (with-temp-file (expand-file-name (car snapshot) directory)
        (insert (cdr snapshot))))
    directory)
  "Directory holding the JSON snapshots these tests read.
The snapshots are the literals above, written to a temporary directory
when this file is loaded, so the suite builds the JSON it depends on
instead of shipping snapshot files.")

(defconst llm-pick-source-test--fixture
  (expand-file-name "benchlm-sample.json" llm-pick-source-test--fixture-directory)
  "Snapshot of the capability source used by these tests.")

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

(ert-deftest llm-pick-source-test-fixture-loader-capability ()
  (let ((entries (llm-pick-source--fixture-loader
                  (list :kind 'capability
                        :category "coding"
                        :fixture llm-pick-source-test--fixture))))
    (ert-info ("Every model becomes an entry; (id score providers)")
      (should (equal (mapcar (lambda (entry)
                               (list (plist-get entry :id)
                                     (plist-get entry :score)
                                     (plist-get entry :providers)))
                             entries)
                     '(("claude-3.5-sonnet" 88 ((anthropic . "claude-3-5-sonnet-20241022")))
                       ("gpt-4o" 92 ((openai . "gpt-4o")))
                       ("gpt-4o-mini" 78 ((openai . "gpt-4o-mini")))
                       ("gemini-1.5-flash" 82 nil)
                       ("llama-3.1-8b" 65 nil)
                       ("orphan-model" 70 nil)))))))

(ert-deftest llm-pick-source-test-fixture-loader-price ()
  (let ((entries (llm-pick-source--fixture-loader
                  (list :kind 'price
                        :fixture (expand-file-name
                                  "openrouter-sample.json"
                                  llm-pick-source-test--fixture-directory)))))
    (ert-info ("A price entry carries :in and :out per million tokens")
      (should (equal (mapcar (lambda (entry)
                               (list (plist-get entry :id)
                                     (plist-get entry :prices)))
                             entries)
                     '(("anthropic/claude-3.5-sonnet" (:in 3.0 :out 15.0))
                       ("openai/gpt-4o" (:in 5.0 :out 15.0))
                       ("openai/gpt-4o-mini" (:in 0.15 :out 0.6))
                       ("google/gemini-flash-1.5" (:in 0.075 :out 0.3))
                       ("meta-llama/llama-3.1-8b-instruct" (:in 0.05 :out 0.1))
                       ("qwen/qwen-2.5-72b" (:in 0.35 :out 0.4))))))))

(ert-deftest llm-pick-source-test-fixture-loader-missing-file ()
  (ert-info ("A missing snapshot is a loud error, not an empty result")
    (should-error (llm-pick-source--fixture-loader
                   (list :kind 'capability
                         :fixture (expand-file-name
                                   "no-such-snapshot.json"
                                   llm-pick-source-test--fixture-directory)))
                  :type 'llm-pick-error)))

(ert-deftest llm-pick-source-test-collect-merges-sources ()
  (let ((llm-pick-source-fixture-directory llm-pick-source-test--fixture-directory)
        ;; The suite never opens a socket: the snapshots are the data.
        (llm-pick-source-offline t)
        (llm-pick-core-default-capability-source 'benchlm)
        (llm-pick-core-default-price-source 'openrouter)
        (llm-pick-align-match-threshold 0.85)
        (llm-pick-align-match-ambiguity-gap 0.05)
        (llm-pick-align-on-unmatched 'standalone))
    (let ((records (llm-pick-source--collect :sources '(benchlm openrouter)
                                      :category "coding")))
      (ert-info ("One record per canonical ID; (name score or-out scope)")
        (should (equal (mapcar (lambda (record)
                                 (list (llm-pick-core--field record 'name)
                                       (llm-pick-core--field record 'score)
                                       (llm-pick-core--field record 'or-out)
                                       (llm-pick-core--field record 'scope)))
                               records)
                       '(("claude-3-5-sonnet" 88 15.0 both)
                         ("gemini-1-5-flash" 82 0.3 both)
                         ("gpt-4o" 92 15.0 both)
                         ("gpt-4o-mini" 78 0.6 both)
                         ("llama-3-1-8b" 65 0.1 both)
                         ("orphan-model" 70 nil capability-only)
                         ("qwen-2-5-72b" nil 0.4 price-only)))))
      (ert-info ("Provider IDs are merged across sources; (provider . id)")
        (should (equal (plist-get (car (cl-remove-if-not
                                        (lambda (record)
                                          (equal (llm-pick-core--field record 'name)
                                                 "gpt-4o"))
                                        records))
                                  :providers)
                       '((openai . "gpt-4o")
                         (openrouter . "openai/gpt-4o"))))))))

(ert-deftest llm-pick-source-test-collect-category-selects-score ()
  (let ((llm-pick-source-fixture-directory llm-pick-source-test--fixture-directory)
        (llm-pick-source-offline t)
        (llm-pick-core-default-capability-source 'benchlm)
        (llm-pick-core-default-price-source 'openrouter))
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
        (should (equal (list coding math best) '(88 80 88)))))))

(ert-deftest llm-pick-source-test-collect-without-capability-source ()
  (let ((llm-pick-source-fixture-directory llm-pick-source-test--fixture-directory)
        (llm-pick-source-offline t)
        (llm-pick-core-default-capability-source 'benchlm)
        (llm-pick-core-default-price-source 'openrouter)
        (llm-pick-align-on-unmatched 'standalone))
    (let ((records (llm-pick-source--collect :sources '(openrouter))))
      (ert-info ("Without a capability source every record is price-only")
        (should (equal (delete-dups (mapcar (lambda (record)
                                              (llm-pick-core--field record 'scope))
                                            records))
                       '(price-only)))))))

(ert-deftest llm-pick-source-test-collect-two-categories-keys-the-scores ()
  (let ((llm-pick-source-fixture-directory llm-pick-source-test--fixture-directory)
        (llm-pick-source-offline t)
        (llm-pick-core-default-capability-source 'benchlm)
        (llm-pick-core-default-price-source 'openrouter)
        (llm-pick-align-match-threshold 0.85)
        (llm-pick-align-match-ambiguity-gap 0.05)
        (llm-pick-align-on-unmatched 'standalone))
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
        (should (equal (llm-pick-core--field claude 'score) 88))))))

(ert-deftest llm-pick-source-test-collect-bad-category-signals ()
  (ert-info ("A category that is not a name is a loud error")
    (should-error (llm-pick-source--collect :sources '(benchlm) :category 7)
                  :type 'llm-pick-error)))

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

(ert-deftest llm-pick-source-test-benchlm-url-carries-the-category ()
  (ert-info ("The category is a query parameter, the limit its maximum")
    (should (equal (llm-pick-source--benchlm-url "coding")
                   "https://benchlm.ai/api/data/leaderboard?limit=200&category=coding"))
    (should (equal (llm-pick-source--benchlm-url nil)
                   "https://benchlm.ai/api/data/leaderboard?limit=200"))))

(ert-deftest llm-pick-source-test-benchlm-loader-reads-the-leaderboard ()
  (cl-letf (((symbol-function 'llm-pick-fetch-get-http)
             (lambda (&rest _) llm-pick-source-test--leaderboard)))
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
  (cl-letf (((symbol-function 'llm-pick-fetch-get-http)
             (lambda (&rest _) llm-pick-source-test--model-list)))
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

(ert-deftest llm-pick-source-test-collect-picks-the-fetcher-when-online ()
  (let ((llm-pick-source-fixture-directory llm-pick-source-test--fixture-directory)
        (llm-pick-core-default-capability-source 'benchlm)
        (llm-pick-core-default-price-source 'openrouter)
        (llm-pick-source-offline nil)
        (fetched 0)
        (snapshotted 0))
    ;; Stub the two loaders, never `llm-pick-source--collect-source': the
    ;; choice between them is what this test is about, so it has to run.
    (cl-letf (((symbol-function 'llm-pick-source--benchlm-loader)
               (lambda (_options)
                 (cl-incf fetched)
                 '((:id "claude-3.5-sonnet" :display-name "Claude" :score 88))))
              ((symbol-function 'llm-pick-source--fixture-loader)
               (lambda (_options) (cl-incf snapshotted) nil)))
      (llm-pick-source--collect :sources '(benchlm) :category "coding")
      (ert-info ("A source with a fetcher reads its service, not its snapshot")
        (should (= fetched 1))
        (should (= snapshotted 0))))))

(ert-deftest llm-pick-source-test-collect-picks-the-snapshot-when-offline ()
  (let ((llm-pick-source-fixture-directory llm-pick-source-test--fixture-directory)
        (llm-pick-core-default-capability-source 'benchlm)
        (llm-pick-core-default-price-source 'openrouter)
        (llm-pick-source-offline t)
        (fetched 0))
    (cl-letf (((symbol-function 'llm-pick-source--benchlm-loader)
               (lambda (_options) (cl-incf fetched) nil)))
      (let ((records (llm-pick-source--collect :sources '(benchlm) :category "coding")))
        (ert-info ("Offline the snapshot answers and no socket is opened")
          (should (= fetched 0))
          (should (= (length records) 6)))))))

(ert-deftest llm-pick-source-test-collect-unknown-source-signals ()
  (ert-info ("A typo in a source name must not be answered with an empty list")
    (should-error (llm-pick-source--collect :sources '(nope))
                  :type 'llm-pick-error)))

(provide 'llm-pick-source-test)

;;; llm-pick-source-test.el ends here
