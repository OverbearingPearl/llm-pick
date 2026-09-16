;;; llm-pick-report-test.el --- Tests for the report commands -*- lexical-binding: t; -*-

;;; Commentary:

;;
;; The report commands are the one place where collection, selection and
;; rendering meet, so they are tested end to end against the bundled
;; snapshots: the expected text is the text a user reads.  That makes the
;; layout a contract, which is the point - a change to a column or to the
;; headline is then a deliberate change to this file.
;;
;; `:display nil' keeps the tests free of buffers.  The test that checks
;; this compares the `*llm-pick*' buffer before and after instead of
;; assuming the user has none open.
;;
;; Every test binds the options it depends on, the source options
;; included, so the suite does not depend on the configuration of the
;; running Emacs.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'llm-pick)

(defconst llm-pick-report-test--second-price-source
  (cons 'bedrock
        '(:kind price
          :description "Second channel, registered by this test only"
          :loader llm-pick-source--fixture-loader
          :fixture "{\"models\":[{\"id\":\"claude-3.5-sonnet\",\"pricing\":{\"prompt\":3.0,\"completion\":12.0}},{\"id\":\"gpt-4o\",\"pricing\":{\"prompt\":4.0,\"completion\":12.0}},{\"id\":\"gpt-4o-mini\",\"pricing\":{\"prompt\":0.2,\"completion\":0.7}},{\"id\":\"gemini-1.5-flash\",\"pricing\":{\"prompt\":0.08,\"completion\":0.3}},{\"id\":\"llama-3.1-8b\",\"pricing\":{\"prompt\":0.04,\"completion\":0.08}}]}"))
  "A second price source, registered inside one test and nowhere else.")

(defconst llm-pick-report-test--coding-budget
  (concat "=== coding | 4 models | budget $3.00 ===\n"
          "\n"
          "Model" (make-string 11 ?\s) "  "
          "Capability" (make-string 20 ?\s) "  "
          "Score  $/M out\n"
          "gemini-1-5-flash  " (make-string 30 ?█) "     82      0.3\n"
          "gpt-4o-mini" (make-string 5 ?\s) "  "
          (make-string 29 ?█) "░     78      0.6\n"
          "llama-3-1-8b" (make-string 4 ?\s) "  "
          (make-string 24 ?█) "░░░░░░     65      0.1\n"
          "qwen-2-5-72b" (make-string 4 ?\s) "  "
          (make-string 30 ?░) "      —      0.4\n")
  "Report of the coding models under $3.00 per million output tokens.
The bars scale against the best model of the report, 82, so the first
bar is full and a model without a score keeps an empty one.")

(defmacro llm-pick-report-test--with-fixtures (&rest body)
  "Run BODY with the options the fixture reports depend on bound."
  (declare (indent 0))
  `(let (;; The suite never opens a socket: the snapshots are the data.
         (llm-pick-source-offline t)
         (llm-pick-core-default-capability-source 'benchlm)
         (llm-pick-core-default-price-source 'openrouter)
         (llm-pick-report-columns '(name bar score or-out))
         (llm-pick-report-ladder-bounds '(0.5 1 2 5 nil))
         (llm-pick-render-report-default-bar-width 30)
         (llm-pick-align-match-threshold 0.85)
         (llm-pick-align-match-ambiguity-gap 0.05)
         (llm-pick-align-on-unmatched 'standalone))
     ,@body))

(ert-deftest llm-pick-report-test-coding-budget-matches-the-fixtures ()
  (llm-pick-report-test--with-fixtures
    (ert-info ("The report is the headline, the table and a trailing newline")
      (should (equal (llm-pick-report :category "coding" :budget 3.0
                                      :display nil)
                     llm-pick-report-test--coding-budget)))))

(ert-deftest llm-pick-report-test-display-nil-does-not-touch-a-buffer ()
  (llm-pick-report-test--with-fixtures
    (let ((existing (get-buffer "*llm-pick*")))
      (ert-info ("Asking for the text text must not create or reuse a buffer")
        (should (equal (llm-pick-report :category "coding" :budget 3.0
                                        :display nil)
                       llm-pick-report-test--coding-budget))
        (should (eq (get-buffer "*llm-pick*") existing))))))

(ert-deftest llm-pick-report-test-unknown-argument-signals ()
  ;; The mode check runs after the records are collected, so this test
  ;; needs the snapshots like any other report test.  Without them the
  ;; second case reached the services and blocked the suite.
  (llm-pick-report-test--with-fixtures
    (ert-info ("A typo in an argument must not be answered with an unfiltered report")
      (should-error (llm-pick-report :budjet 3.0 :display nil)
                    :type 'llm-pick-error))
    (ert-info ("A typo in a mode is a loud error, not a table")
      (should-error (llm-pick-report :mode 'furthest :display nil)
                    :type 'llm-pick-error))))

(ert-deftest llm-pick-report-test-frontier-mode ()
  (llm-pick-report-test--with-fixtures
    (let ((text (llm-pick-report :category "coding" :mode 'frontier
                                 :display nil)))
      (ert-info ("The frontier report carries the gain of every step")
        (should (string-match-p "Gain/\\$" text)))
      (ert-info ("The samples leave three undominated models: llama, gemini, gpt-4o")
        (should (string-match-p "llama-3-1-8b" text))
        (should (string-match-p "gemini-1-5-flash" text))
        (should (string-match-p "gpt-4o" text))
        (should-not (string-match-p "gpt-4o-mini" text)))
      (ert-info ("17 points for $0.20/M more is 85 points per dollar")
        (should (string-match-p "\\+85\\.0" text))))))

(ert-deftest llm-pick-report-test-ladder-mode ()
  (llm-pick-report-test--with-fixtures
    (let ((text (llm-pick-report :category "coding" :mode 'ladder
                                 :bounds '(1.0 nil) :display nil)))
      (ert-info ("Both buckets are shown, the open one under its own label")
        (should (string-match-p "Up to \\$/M out" text))
        (should (string-match-p "open" text)))
      (ert-info ("The $1 bucket holds the strongest model below it")
        (should (string-match-p "gemini-1-5-flash" text))))))

(ert-deftest llm-pick-report-test-pick-returns-the-best-under-a-budget ()
  (llm-pick-report-test--with-fixtures
    (ert-info ("Below $1/M out the most capable model is gemini-1-5-flash")
      (should (equal (llm-pick-pick :category "coding" :budget 1.0)
                     "gemini-1-5-flash")))
    (ert-info ("Reaching 80 points is cheapest with gemini-1-5-flash")
      (should (equal (llm-pick-pick :category "coding" :target-score 80)
                     "gemini-1-5-flash")))
    (ert-info ("A provider filter guarantees the choice can be called")
      (should (equal (llm-pick-pick :category "coding" :budget 1.0
                                    :available-on '(openai))
                     "gpt-4o-mini")))
    (ert-info ("Nothing affordable is an error, not nil")
      (should-error (llm-pick-pick :category "coding" :budget 0.01)
                    :type 'llm-pick-error))))

(ert-deftest llm-pick-report-test-resolve-maps-back-to-a-provider-id ()
  (llm-pick-report-test--with-fixtures
    (ert-info ("The canonical ID resolves to the provider's own ID")
      (should (equal (llm-pick-resolve "gpt-4o" 'openai :category "coding")
                     '(openai . "gpt-4o"))))
    (ert-info ("The first provider of the list that names the model wins")
      (should (equal (llm-pick-resolve-with-fallback
                      "claude-3-5-sonnet" '(bedrock anthropic openrouter)
                      :category "coding")
                     '(anthropic . "claude-3-5-sonnet-20241022"))))
    (ert-info ("A provider that does not carry the model is an error")
      (should-error (llm-pick-resolve "gpt-4o" 'anthropic :category "coding")
                    :type 'llm-pick-error))))

(ert-deftest llm-pick-report-test-align-report-shows-the-last-alignment ()
  (llm-pick-report-test--with-fixtures
    (let ((llm-pick-align--last-report nil))
      (ert-info ("Without an alignment there is nothing to report")
        (should-error (llm-pick-align-report-text) :type 'llm-pick-error))
      (llm-pick-collect :category "coding")
      (ert-info ("Collecting aligns, and the report describes that alignment")
        (should (string-match-p "\\`=== ID alignment ==="
                                (llm-pick-align-report-text)))))))

(ert-deftest llm-pick-report-test-providers-is-a-spelling-of-available-on ()
  (llm-pick-report-test--with-fixtures
    (ert-info (":providers selects exactly what :available-on selects")
      (should (equal (llm-pick-pick :category "coding" :budget 3.0
                                    :providers '(openai))
                     (llm-pick-pick :category "coding" :budget 3.0
                                    :available-on '(openai)))))))

(ert-deftest llm-pick-report-test-two-categories-side-by-side ()
  (llm-pick-report-test--with-fixtures
    (let ((text (llm-pick-report
                 :category '("coding" "math")
                 :columns '(name score (score benchlm coding)
                            (score benchlm math))
                 :display nil)))
      (ert-info ("The headline names both categories")
        (should (string-match-p (regexp-quote "=== coding + math |") text)))
      (ert-info ("Each category gets a column of its own")
        (should (string-match-p "Score benchlm coding" text))
        (should (string-match-p "Score benchlm math" text))))))

(ert-deftest llm-pick-report-test-gap-needs-a-registered-second-source ()
  (llm-pick-report-test--with-fixtures
    (ert-info ("Without a second source the gap column is empty")
      (should (string-match-p "—"
                              (llm-pick-report :category "coding"
                                               :columns '(name gap)
                                               :display nil))))
    ;; The registry and the option are bound here, so the command below
    ;; behaves as it does for a user who registered the source himself.
    (let ((llm-pick-source-sources (append llm-pick-source-sources
                                    (list llm-pick-report-test--second-price-source)))
          (llm-pick-core-secondary-price-source 'bedrock))
      (let ((text (llm-pick-report :category "coding"
                                   :where '((> gap 0.1))
                                   :columns '(name or-out bm-out gap)
                                   :display nil)))
        (ert-info ("Only the models whose channels differ enough show up")
          (should (string-match-p "Gap" text))
          (should (string-match-p "gpt-4o-mini" text))
          (should-not (string-match-p "qwen" text)))
        (ert-info ("$15 against $12 is a 25% gap")
          (should (string-match-p "25%" text)))))))

(ert-deftest llm-pick-report-test-query-line-becomes-an-argument-plist ()
  (llm-pick-report-test--with-fixtures
    (let ((llm-pick-query-read-last-query nil))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _)
                   (concat "category coding budget 3 score 80 "
                           "on openai,anthropic order value top 5 "
                           "where score>75 mode frontier"))))
        (ert-info ("Every word of the line has a reader")
          (should (equal (llm-pick-query-read-args "Report query: ")
                         '(:category "coding" :budget 3 :target-score 80
                           :available-on (openai anthropic)
                           :order value :top 5
                           :where ((> score 75)) :mode frontier))))))))

(ert-deftest llm-pick-report-test-query-line-rejects-a-typo ()
  (llm-pick-report-test--with-fixtures
    (let ((llm-pick-query-read-last-query nil))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "budjet 3")))
        (ert-info ("A misspelled word must not be dropped in silence")
          (should-error (llm-pick-query-read-args "Report query: ")
                        :type 'llm-pick-error))))))

(ert-deftest llm-pick-report-test-a-report-asks-nothing-without-a-prefix ()
  (llm-pick-report-test--with-fixtures
    (ert-info ("Without a prefix argument the interactive spec reads nothing")
      (let ((asked nil))
        (cl-letf (((symbol-function 'llm-pick-query-read-args)
                   (lambda (&rest _) (setq asked t) nil)))
          (should (equal (llm-pick--report-interactive nil) nil))
          (should-not asked))))
    (ert-info ("With a prefix argument it reads the query")
      (cl-letf (((symbol-function 'llm-pick-query-read-args)
                 (lambda (&rest _) '(:category "coding"))))
        (should (equal (llm-pick--report-interactive '(4))
                       '(:category "coding")))))))

(ert-deftest llm-pick-report-test-report-query-reports-every-model ()
  (llm-pick-report-test--with-fixtures
    (let ((shown nil))
      (cl-letf (((symbol-function 'llm-pick-query-read-args)
                 (lambda (&rest _) nil))
                ((symbol-function 'llm-pick--report-display)
                 (lambda (text) (setq shown text) text)))
        (call-interactively #'llm-pick-report-query)
        (ert-info ("An empty query is a report over every model")
          (should (string-match-p "\\`=== all categories" shown)))))))

(ert-deftest llm-pick-report-test-query-completion-offers-words-then-values ()
  (with-temp-buffer
    (ert-info ("An empty line offers every word")
      (should (equal (llm-pick-query-read-candidates (point))
                     (mapcar #'car llm-pick-query-read-words))))
    (ert-info ("After a word the line offers that word's values")
      (erase-buffer)
      (insert "mode ")
      (should (equal (llm-pick-query-read-candidates (point))
                     '("table" "frontier" "ladder"))))
    (ert-info ("A category offers the real category names")
      (erase-buffer)
      (insert "category ")
      (should (llm-pick-query-read-candidates (point))))
    (ert-info ("A source, an anchor or a provider offers the registered names")
      (dolist (word '("sources " "anchor " "on "))
        (erase-buffer)
        (insert word)
        (should (equal (llm-pick-query-read-candidates (point))
                       (llm-pick-source--names)))))
    (ert-info ("A column list offers the fields plus the bar")
      (erase-buffer)
      (insert "columns ")
      (should (member "bar" (llm-pick-query-read-candidates (point))))
      (should (member "or-out" (llm-pick-query-read-candidates (point)))))
    (ert-info ("A number like budget offers no value to start from")
      (erase-buffer)
      (insert "budget ")
      (should (equal (llm-pick-query-read-candidates (point)) nil)))))

(ert-deftest llm-pick-report-test-query-completion-replaces-the-token ()
  (with-temp-buffer
    (insert "mode tab")
    (goto-char (point-max))
    (ert-info ("TAB replaces the token at point with the chosen candidate")
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "table")))
        (llm-pick-query-read-complete))
      (should (equal (buffer-string) "mode table")))))

(ert-deftest llm-pick-report-test-query-minibuffer-map-tab-completes ()
  (ert-info ("TAB in the query minibuffer completes, it does not self-insert")
    (should (eq (lookup-key llm-pick-query-read-minibuffer-map "\t")
                #'llm-pick-query-read-complete))))

(ert-deftest llm-pick-report-test-query-word-help-covers-every-word ()
  (ert-info ("Every word a query accepts also explains itself")
    (dolist (entry llm-pick-query-read-words)
      (should (stringp (car entry)))
      (should (keywordp (nth 1 entry)))
      (should (stringp (nth 2 entry)))
      (should (llm-pick-query-read-key (car entry)))))
  (ert-info ("Every word with a closed value set offers candidates")
    (dolist (entry llm-pick-query-read-words)
      (unless (member (nth 1 entry) '(:category :budget :target-score))
        (should (llm-pick-query-read-values (nth 1 entry)))))))

(ert-deftest llm-pick-report-test-align-check-shows-a-conflict-instead ()
  ;; The alignment stops at the first problem, which is right for a
  ;; report and wrong for a repair: the check command walks the whole
  ;; catalogue and shows what it found.
  (llm-pick-report-test--with-fixtures
    (let ((llm-pick-source-sources
           (list (cons 'benchlm
                       (list :kind 'capability
                             :loader (lambda (_options)
                                       '((:id "foo-bar" :score 80)
                                         (:id "foo_bar" :score 70)))))))
          (shown nil))
      (ert-info ("A plain collection stops at the conflict")
        (should-error (llm-pick-collect) :type 'llm-pick-align-conflict))
      (cl-letf (((symbol-function 'llm-pick--report-display)
                 (lambda (text) (setq shown text) text)))
        (ert-info ("The check command reports it and returns the text")
          (should (string-match-p "ID alignment conflict"
                                  (call-interactively #'llm-pick-align-check))))
        (should (string-match-p "foo_bar" shown))))
    (ert-info ("Nothing is left in the session problem list")
      (should (equal llm-pick-align--problems nil)))))

(provide 'llm-pick-report-test)

;;; llm-pick-report-test.el ends here
