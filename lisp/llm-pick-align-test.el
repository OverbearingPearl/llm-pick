;;; llm-pick-align-test.el --- Tests for llm-pick-align -*- lexical-binding: t; -*-

;;; Commentary:

;;
;; Alignment tests are data driven: every time a new naming scheme is
;; taught to `llm-pick-normalize-rules', one more pair is added to the
;; known pairs below.  Failure output lists the pairs that broke with
;; both normalizations next to them.

;;; Code:

(require 'ert)
(require 'llm-pick-align)

(ert-deftest llm-pick-align-test-normalize-idempotent ()
  (let ((ids '("anthropic/claude-3.5-sonnet"
               "openai/gpt-4o"
               "meta-llama/Llama-3.1-70B-Instruct"
               "gpt-4o-2024-05-13"
               "claude-3-5-sonnet-20241022:beta"
               "Tencent/Hy3 Preview"
               "qwen/qwen3.8-max-0902"
               "thinkingmachines/inkling"
               "google/gemma-4-26b-a4b-it"))
        (broken nil))
    (dolist (id ids)
      (let ((once (llm-pick-align--normalize id))
            (twice (llm-pick-align--normalize (llm-pick-align--normalize id))))
        (unless (equal once twice)
          (push (list id once twice) broken))))
    (ert-info ("A normalized ID must normalize to itself; broken: (id once twice)")
      (should (equal broken nil)))))

(ert-deftest llm-pick-align-test-known-pairs ()
  ;; Pairs that only differ in token order (`gemini-pro-1.5' against
  ;; `gemini-1.5-pro') are the similarity layer's job, not the
  ;; normalization layer's; they are covered by the reordered token test
  ;; below.
  (let ((pairs '(("claude-3.5-sonnet" . "anthropic/claude-3.5-sonnet")
                 ("claude-3.5-sonnet" . "claude-3-5-sonnet-20241022")
                 ("gpt-4o" . "openai/gpt-4o")
                 ("gpt-4o" . "gpt-4o-2024-05-13")
                 ("llama-3.1-8b" . "meta-llama/Llama-3.1-8B-Instruct")
                 ;; The vendor pairs below come from the live catalogues:
                 ;; the leaderboard spells the brand, the price catalogue
                 ;; the slug.  Left alone the vendor stayed part of the
                 ;; ID and the two never met, which is what put a few
                 ;; hundred entries in the unmatched list.
                 ("Alibaba/Qwen3.8 Max" . "qwen/qwen3.8-max-0902")
                 ("xAI/Grok 4.6" . "x-ai/grok-4.6")
                 ("Moonshot AI/Kimi K3" . "moonshotai/kimi-k3")
                 ("Thinking Machines Lab/Inkling" . "thinkingmachines/inkling")
                 ;; `-it' is the instruction tuned marker of the Gemma
                 ;; family, the same kind of suffix as `-instruct'.
                 ("Google/Gemma 4 26B A4B" . "google/gemma-4-26b-a4b-it")))
        (broken nil))
    (dolist (pair pairs)
      (let ((left (llm-pick-align--normalize (car pair)))
            (right (llm-pick-align--normalize (cdr pair))))
        (unless (equal left right)
          (push (list pair left right) broken))))
    (ert-info ("Equivalent IDs must normalize alike; broken: (pair left right)")
      (should (equal broken nil)))))

(ert-deftest llm-pick-align-test-preview-is-part-of-the-name ()
  ;; BenchLM lists `Tencent/Hy3' and `Tencent/Hy3 Preview' as two models
  ;; of its own.  Dropping `-preview' as a release suffix made them
  ;; normalize alike, and the collision stopped every report against the
  ;; live leaderboard, because two anchor IDs then wanted one canonical
  ;; ID.
  (let ((llm-pick-core-default-capability-source 'benchlm))
    (ert-info ("A name and its preview normalize apart")
      (should (equal (llm-pick-align--normalize "Tencent/Hy3") "tencent-hy3"))
      (should (equal (llm-pick-align--normalize "Tencent/Hy3 Preview")
                     "tencent-hy3-preview")))
    (ert-info ("The anchor keeps both as canonical IDs of their own")
      (should (equal (plist-get (llm-pick-align--align
                                 '((benchlm . ("Tencent/Hy3"
                                               "Tencent/Hy3 Preview"))))
                                :mapping)
                     '(((benchlm . "Tencent/Hy3") . "tencent-hy3")
                       ((benchlm . "Tencent/Hy3 Preview")
                        . "tencent-hy3-preview")))))))

(ert-deftest llm-pick-align-test-a-version-is-part-of-the-name ()
  ;; BenchLM lists `DeepSeek/DeepSeek V3', `V3.1' and `V3.2' as three
  ;; models of its own.  Dropping the version as a release suffix made all
  ;; three normalize to `deepseek', and the collision stopped every report
  ;; against the live leaderboard.  `deepseek' is also the wrong name for
  ;; the release: it is the first model of the family, not the third.
  (ert-info ("Three releases normalize to three canonical IDs")
    (should (equal (mapcar #'llm-pick-align--normalize
                           '("DeepSeek/DeepSeek V3"
                             "DeepSeek/DeepSeek V3.1"
                             "DeepSeek/DeepSeek V3.2"))
                   '("deepseek-v3" "deepseek-v3-1" "deepseek-v3-2"))))
  (ert-info ("The anchor keeps all three instead of reporting a conflict")
    (should (equal (length (plist-get (llm-pick-align--align
                                       '((benchlm . ("DeepSeek/DeepSeek V3"
                                                     "DeepSeek/DeepSeek V3.1"
                                                     "DeepSeek/DeepSeek V3.2"))))
                                      :mapping))
                   3))))

(ert-deftest llm-pick-align-test-a-short-date-is-a-date ()
  ;; Both catalogues date their snapshots, and the short form is four
  ;; digits, `MMDD'.  The rule that drops a trailing release number looked
  ;; for six to eight of them, so `deepseek-v4-pro-0813' stayed a model of
  ;; its own while `deepseek-v4-pro' sat on the anchor, and the live
  ;; catalogues came back with a few hundred unmatched IDs.
  (ert-info ("A four digit date goes the way of a long one")
    (should (equal (llm-pick-align--normalize "deepseek-v4-pro-0813")
                   "deepseek-v4-pro"))
    (should (equal (llm-pick-align--normalize "mistralai/mistral-large-2407")
                   "mistral-large"))
    (should (equal (llm-pick-align--normalize "openai/gpt-4o-mini-2024-07-18")
                   "gpt-4o-mini")))
  (ert-info ("The two names meet on one canonical ID, with no threshold involved")
    (should (equal (mapcar #'llm-pick-align--normalize
                           '("deepseek-v4-pro" "deepseek/deepseek-v4-pro-0813"))
                   '("deepseek-v4-pro" "deepseek-v4-pro"))))
  (ert-info ("A name that ends in a number keeps it")
    ;; The rule wants at least four digits, so a short version number is
    ;; part of the name and never a date.
    (should (equal (llm-pick-align--normalize "openai/gpt-4") "gpt-4"))
    (should (equal (llm-pick-align--normalize "meta-llama/llama-3.1-8b-instruct")
                   "llama-3-1-8b"))))

(ert-deftest llm-pick-align-test-a-generic-name-does-not-match-its-versions ()
  ;; The live catalogues pair a generic OpenRouter name with the versioned
  ;; names BenchLM lists.  A prefix match used to score a constant 0.90
  ;; whatever it covered, so `claude-opus-4' scored 0.900 against both
  ;; `claude-opus-4-7' and `claude-opus-4-8', the two candidates tied, and
  ;; the whole report stopped on an ambiguity that has no answer: only the
  ;; user can say which of the two the generic name means.
  (let ((llm-pick-align-match-threshold 0.85)
        (llm-pick-align-match-ambiguity-gap 0.05)
        (llm-pick-align-on-unmatched 'standalone))
    (ert-info ("A prefix match is only as strong as the part it covers")
      (should (< (llm-pick-align--similarity "claude-opus-4" "claude-opus-4-8")
                 0.85))
      (should (< (llm-pick-align--similarity "gpt-5" "gpt-5-6-sol") 0.5)))
    (ert-info ("So a generic name leaves every versioned sibling unmatched")
      (let ((report (llm-pick-align--align
                     '((benchlm . ("Claude Opus 4 8" "Claude Opus 4 7"))
                       (openrouter . ("anthropic/claude-opus-4")))
                     'benchlm)))
        (should (equal (plist-get report :standalone)
                       '("claude-opus-4")))))))

(ert-deftest llm-pick-align-test-a-gap-at-the-threshold-is-not-ambiguous ()
  ;; 0.95 - 0.90 comes out as 0.04999999999999993, so a bare `<` reported
  ;; a gap of 0.050 as smaller than a threshold of 0.05 and the message
  ;; contradicted itself.  The scores are synthetic here: what this test
  ;; is about is the comparison of two scores, not how the built-in
  ;; functions happen to arrive at them.
  (let ((llm-pick-align-match-threshold 0.85)
        (llm-pick-align-match-ambiguity-gap 0.05)
        (llm-pick-align-on-unmatched 'standalone)
        (llm-pick-align-similarity-fns
         (list (lambda (_candidate canonical)
                 (if (equal canonical "claude-4-sonnet") 0.95 0.90)))))
    (ert-info ("A gap that prints as the threshold is a match, not a tie")
      (should (equal (cdr (assoc '(openrouter . "anthropic/claude-sonnet-4")
                                 (plist-get (llm-pick-align--align
                                             '((benchlm . ("claude-4-sonnet"
                                                           "claude-sonnet-4-6"))
                                               (openrouter . ("anthropic/claude-sonnet-4")))
                                             'benchlm)
                                            :mapping)))
                     "claude-4-sonnet")))))

(ert-deftest llm-pick-align-test-a-vendor-is-dropped-whichever-way-it-is-spelled ()
  ;; Both catalogues put a vendor before the model name, and they spell it
  ;; differently: the leaderboard writes a brand (`xAI/Grok 4.6'), the
  ;; price catalogue a slug (`x-ai/grok-4.6').  Left in the ID the pair
  ;; scored 0.600 against each other, so the vendor rule drops either
  ;; spelling.
  (ert-info ("A brand and a slug of one vendor both go")
    (should (equal (llm-pick-align--normalize "qwen/qwen3.8-max-0902")
                   (llm-pick-align--normalize "Alibaba/Qwen3.8 Max")))
    (should (equal (llm-pick-align--normalize "x-ai/grok-4.6")
                   (llm-pick-align--normalize "xAI/Grok 4.6")))
    (should (equal (llm-pick-align--normalize "moonshotai/kimi-k3")
                   (llm-pick-align--normalize "Moonshot AI/Kimi K3")))
    (should (equal (llm-pick-align--normalize "thinkingmachines/inkling")
                   (llm-pick-align--normalize "Thinking Machines Lab/Inkling"))))
  (ert-info ("The canonical ID is the model name, not the vendor plus it")
    (should (equal (llm-pick-align--normalize "x-ai/grok-4.6") "grok-4-6"))
    (should (equal (llm-pick-align--normalize "qwen/qwen3.8-max-0902")
                   "qwen3-8-max")))
  (ert-info ("A model one source alone lists keeps the name it had")
    ;; Rewriting the slug into the brand instead of dropping both would
    ;; rename this one to `alibaba-qwen-2-5-72b'.  It is a model of the
    ;; price catalogue only, and nothing about it says Alibaba; the live
    ;; sample snapshots and the report tests pin it as `qwen-2-5-72b'.
    (should (equal (llm-pick-align--normalize "qwen/qwen-2.5-72b")
                   "qwen-2-5-72b")))
  (ert-info ("A vendor one source only drops is dropped on both sides")
    (should (equal (llm-pick-align--normalize "openai/gpt-4o") "gpt-4o"))
    (should (equal (llm-pick-align--normalize "OpenAI/GPT-4o") "gpt-4o"))))

(ert-deftest llm-pick-align-test-a-repeated-vendor-is-not-part-of-the-name ()
  ;; `minimax/minimax-m2' names its vendor twice, and so does the
  ;; `Minimax/Minimax M2.7' that BenchLM lists.  Keeping the repetition
  ;; gave `minimax-minimax-m2' 18 characters out of the 20 of both
  ;; `minimax-minimax-m2-5' and `minimax-minimax-m2-7', that is 0.855
  ;; against each: above the threshold, equal, and therefore a tie that
  ;; stopped every report.
  (ert-info ("A vendor segment that repeats is dropped once")
    (should (equal (llm-pick-align--normalize "minimax/minimax-m2") "minimax-m2"))
    (should (equal (llm-pick-align--normalize "Minimax/Minimax M2.7") "minimax-m2-7"))
    (should (equal (llm-pick-align--normalize "mistral/mistral-large-3")
                   "mistral-large-3")))
  (ert-info ("A segment that does not repeat stays: it is part of the name")
    (should (equal (llm-pick-align--normalize "meta/muse-spark-1.2")
                   "meta-muse-spark-1-2"))
    (should (equal (llm-pick-align--normalize "meta-llama/llama-3.1-8b-instruct")
                   "llama-3-1-8b")))
  (ert-info ("The rule is idempotent however often the segment repeats")
    (should (equal (llm-pick-align--normalize "foo/foo/foo") "foo"))
    (should (equal (llm-pick-align--normalize "foo/foo") "foo"))
    (should (equal (llm-pick-align--normalize "foo/bar") "foo-bar"))))

(ert-deftest llm-pick-align-test-a-generic-family-name-stays-unmatched ()
  (let ((llm-pick-align-match-threshold 0.85)
        (llm-pick-align-match-ambiguity-gap 0.05)
        (llm-pick-align-on-unmatched 'standalone))
    (let ((report (llm-pick-align--align
                   '((benchlm . ("Minimax/Minimax M2.7"
                                 "Minimax/Minimax M2.5"))
                     (openrouter . ("minimax/minimax-m2")))
                   'benchlm)))
      (ert-info ("Every spelling gets a canonical ID of its own")
        (should (equal (plist-get report :mapping)
                       '(((benchlm . "Minimax/Minimax M2.7") . "minimax-m2-7")
                         ((benchlm . "Minimax/Minimax M2.5") . "minimax-m2-5")
                         ((openrouter . "minimax/minimax-m2") . "minimax-m2")))))
      (ert-info ("The family name is a visible standalone, not a tie")
        (should (equal (plist-get report :standalone) '("minimax-m2")))))))

(ert-deftest llm-pick-align-test-conflict-names-the-rule-to-tighten ()
  (ert-info ("The rule that collapses two IDs is reported, not just the fact")
    (should (equal (llm-pick-align--normalize-culprit "foo_bar" "foo-bar")
                   '("_+" . "-"))))
  (ert-info ("The conflict carries the rule, so the user knows what to edit")
    ;; Compare the error data, not the text `error-message-string'
    ;; renders from it: that rendering quotes the data again, so a
    ;; literal string test would be testing the renderer.
    (let ((data (condition-case err
                    (progn (llm-pick-align--align '((benchlm . ("foo-bar"
                                                          "foo_bar")))
                                            'benchlm)
                           nil)
                  (llm-pick-align-conflict (cdr err)))))
      (should (equal (length data) 1))
      (should (string-match-p "both normalize to foo-bar" (car data)))
      (should (string-match-p (regexp-quote "_+") (car data))))))

(ert-deftest llm-pick-align-test-a-collecting-run-reports-every-problem ()
  ;; Only unmistakable misses are used here.  An ID such as `x/foo-bar'
  ;; is a substring of `foo-bar' yet covers 7 of its 8 characters, and
  ;; `llm-pick-align--similarity-substring' scales by that coverage, so it
  ;; reaches only 0.90 * 7/8 = 0.7875, below the threshold, and is an
  ;; unmatched ID too.  That is the intended reading of a near miss, but
  ;; it would make this test report three problems instead of the two it
  ;; is about.
  (let ((llm-pick-align-match-threshold 0.85)
        (llm-pick-align-match-ambiguity-gap 0.05)
        (llm-pick-align-on-unmatched 'error)
        (llm-pick-align--collecting t)
        (llm-pick-align--problems nil))
    (llm-pick-align--align '((benchlm . ("foo-bar" "foo_bar"))
                       (openrouter . ("some/unknown-model-xyz"
                                      "another/unknown-model-abc")))
                     'benchlm)
    (ert-info ("The conflict and both unmatched IDs are reported, in order")
      (should (equal (mapcar (lambda (problem) (plist-get problem :type))
                             (reverse llm-pick-align--problems))
                     '(llm-pick-align-conflict
                       llm-pick-align-unmatched
                       llm-pick-align-unmatched))))
    (ert-info ("Every ID is named, not only the last one of its kind")
      (let ((text (mapconcat (lambda (problem)
                               (plist-get problem :description))
                             llm-pick-align--problems "\n")))
        (should (string-match-p "some/unknown-model-xyz" text))
        (should (string-match-p "another/unknown-model-abc" text))))))

(ert-deftest llm-pick-align-test-a-collecting-run-still-maps-every-id ()
  ;; A collecting run has to produce records as well, so an ID a
  ;; reported collision left out of the anchor index still needs a
  ;; canonical ID of its own.
  (let ((llm-pick-align--collecting t)
        (llm-pick-align--problems nil))
    (let ((report (llm-pick-align--align '((benchlm . ("foo-bar" "foo_bar"))
                                     (openrouter . ("x/foo-bar")))
                                   'benchlm)))
      (ert-info ("Every input ID has a mapping entry, in input order")
        (should (equal (mapcar #'car (plist-get report :mapping))
                       '((benchlm . "foo-bar") (benchlm . "foo_bar")
                         (openrouter . "x/foo-bar")))))
      (ert-info ("The colliding IDs share the canonical ID of the first")
        (should (equal (cdr (assoc '(benchlm . "foo_bar")
                                   (plist-get report :mapping)))
                       "foo-bar")))
      (ert-info ("The colliding anchor IDs share one canonical ID, the rest keep their own")
        ;; `x/foo-bar' is a substring of `foo-bar' but covers only 7 of its
        ;; 8 characters, which the threshold rejects, so it stays a model
        ;; of its own instead of becoming a second name for `foo-bar'.
        (should (equal (delete-dups (mapcar #'cdr (plist-get report :mapping)))
                       '("foo-bar" "x-foo-bar")))))))

(ert-deftest llm-pick-align-test-sources-that-agree-settle-a-near-miss ()
  ;; The leaderboard calls it `Mistral Large 3', the stores call it
  ;; `mistral-large', a date they strip on the way in.  The prefix match
  ;; scores 0.95 * 13/15 = 0.823, just under the threshold, so the score
  ;; alone leaves the pair apart.  Two stores spelling it the same way is
  ;; the evidence the score cannot see.
  (let ((llm-pick-align-match-threshold 0.85)
        (llm-pick-align-match-ambiguity-gap 0.05)
        (llm-pick-align-match-consensus-sources 2)
        (llm-pick-align-match-consensus-band 0.05)
        (llm-pick-align-on-unmatched 'error))
    (let ((sources '((benchlm . ("Mistral/Mistral Large 3"))
                     (openrouter . ("mistralai/mistral-large-2512"))
                     (bedrock . ("mistral-large")))))
      (ert-info ("The name two stores agree on is the leaderboard's model")
        (should (equal (plist-get (llm-pick-align--align sources 'benchlm) :mapping)
                       '(((benchlm . "Mistral/Mistral Large 3") . "mistral-large-3")
                         ((openrouter . "mistralai/mistral-large-2512")
                          . "mistral-large-3")
                         ((bedrock . "mistral-large") . "mistral-large-3")))))
      (ert-info ("Nothing is left standalone, and the join is reported")
        (let ((report (llm-pick-align--align sources 'benchlm)))
          (should (equal (plist-get report :standalone) nil))
          (should (equal (plist-get report :promoted)
                         '((:norm "mistral-large" :canonical "mistral-large-3"
                            :sources (openrouter bedrock)))))))
      (ert-info ("Raising the number of agreeing sources turns the rule off")
        ;; With the rule off the near miss is an unmatched ID again, and
        ;; this test still binds `llm-pick-align-on-unmatched' = error from its
        ;; outer let, so the turn-off has to switch that to `standalone'
        ;; as well: the two options have to move together to see the
        ;; silent fallback the rule was standing in for.
        (let ((llm-pick-align-match-consensus-sources 3)
              (llm-pick-align-on-unmatched 'standalone))
          (let ((report (llm-pick-align--align sources 'benchlm)))
            (should (equal (plist-get report :promoted) nil))
            (should (equal (plist-get report :standalone)
                           '("mistral-large")))))
        (ert-info ("`on-unmatched' = error is then loud again")
          (let ((llm-pick-align-match-consensus-sources 3))
            (should-error (llm-pick-align--align sources 'benchlm)
                          :type 'llm-pick-align-unmatched)))))))

(ert-deftest llm-pick-align-test-a-lone-near-miss-stays-unmatched ()
  ;; The same near miss from one store only.  A single source spelling a
  ;; name is exactly what the similarity score already says, so nothing
  ;; is promoted and the ID stays a model of its own.
  (let ((llm-pick-align-match-threshold 0.85)
        (llm-pick-align-match-ambiguity-gap 0.05)
        (llm-pick-align-match-consensus-sources 2)
        (llm-pick-align-match-consensus-band 0.05)
        (llm-pick-align-on-unmatched 'standalone))
    (let ((report (llm-pick-align--align
                   '((benchlm . ("Mistral/Mistral Large 3"))
                     (openrouter . ("mistralai/mistral-large-2512")))
                   'benchlm)))
      (ert-info ("The score decides, as it did before the rule existed")
        (should (equal (plist-get report :promoted) nil))
        (should (equal (plist-get report :standalone) '("mistral-large")))
        (should (equal (cdr (assoc '(openrouter . "mistralai/mistral-large-2512")
                                   (plist-get report :mapping)))
                       "mistral-large"))))))

(ert-deftest llm-pick-align-test-agreement-does-not-lower-the-threshold ()
  ;; Two stores can agree on a spelling that is nowhere near the
  ;; leaderboard's, and then it is a model of theirs, not a name for the
  ;; leaderboard's.  The band is what draws that line.
  (let ((llm-pick-align-match-threshold 0.85)
        (llm-pick-align-match-ambiguity-gap 0.05)
        (llm-pick-align-match-consensus-sources 2)
        (llm-pick-align-match-consensus-band 0.05)
        (llm-pick-align-on-unmatched 'standalone))
    (let ((report (llm-pick-align--align
                   '((benchlm . ("Mistral/Mistral Large 3"))
                     (openrouter . ("mistralai/mistral-large-embed"))
                     (bedrock . ("mistral-large-embed")))
                   'benchlm)))
      (ert-info ("The agreed spelling stays a model of its own")
        (should (equal (plist-get report :promoted) nil))
        (should (equal (plist-get report :standalone)
                       '("mistral-large-embed"))))))
  (ert-info ("A band of zero is the rule switched off")
    (let ((llm-pick-align-match-threshold 0.85)
          (llm-pick-align-match-ambiguity-gap 0.05)
          (llm-pick-align-match-consensus-sources 2)
          (llm-pick-align-match-consensus-band 0)
          (llm-pick-align-on-unmatched 'standalone))
      (let ((report (llm-pick-align--align
                     '((benchlm . ("Mistral/Mistral Large 3"))
                       (openrouter . ("mistralai/mistral-large-2512"))
                       (bedrock . ("mistral-large")))
                     'benchlm)))
        (should (equal (plist-get report :promoted) nil))
        (should (equal (plist-get report :standalone)
                       '("mistral-large")))))))

(ert-deftest llm-pick-align-test-a-conflict-points-at-the-check-command ()
  (ert-info ("The message says how to see every problem at once")
    (let ((message (error-message-string
                    (condition-case err
                        (progn (llm-pick-align--align '((benchlm . ("foo-bar"
                                                              "foo_bar")))
                                                'benchlm)
                               nil)
                      (llm-pick-align-conflict err)))))
      (should (string-match-p "llm-pick-align-check" message)))))

(ert-deftest llm-pick-align-test-a-token-less-pair-still-ranks ()
  ;; The token rule used to answer 0 when two IDs share no token, and 0
  ;; ends the list of `llm-pick-similarity-fns'.  Every anchor ID then
  ;; scored the same against the input, so the `best match' column of a
  ;; live report showed a flat 0.000 next to a name that was arbitrary.
  (ert-info ("A pair without a shared token falls through to the edit distance")
    (should-not (llm-pick-align--similarity-token "sakana-namazu" "claude-fable-5-1"))
    (should (> (llm-pick-align--similarity "sakana-namazu" "claude-fable-5-1") 0.0)))
  (ert-info ("A pair that does share a token is still the token rule's answer")
    ;; Two tokens shared out of four, which is where the 0.475 the live
    ;; report prints for `claude-3-haiku' against `claude-3-opus' comes
    ;; from: those two really are as close as their shared tokens say,
    ;; and both are under the threshold, which is the point.
    (should (< 0.4 (llm-pick-align--similarity-token "claude-3-haiku" "claude-3-opus")
               0.5))))

(ert-deftest llm-pick-align-test-reordered-tokens-align-by-similarity ()
  (let ((llm-pick-core-default-capability-source 'benchlm)
        (llm-pick-align-match-threshold 0.85)
        (llm-pick-align-match-ambiguity-gap 0.05))
    (ert-info ("`gemini-pro-1.5' and `gemini-1.5-pro' differ only in token order")
      (should (>= (llm-pick-align--similarity (llm-pick-align--normalize "gemini-1.5-pro")
                                        (llm-pick-align--normalize "google/gemini-pro-1.5"))
                  llm-pick-align-match-threshold)))
    (ert-info ("The alignment merges such a pair onto one canonical ID")
      (let ((result (llm-pick-align--align '((benchlm . ("gemini-1.5-pro"))
                                       (openrouter . ("google/gemini-pro-1.5"))))))
        (should (equal (plist-get result :standalone) nil))
        (should (equal (cdr (assoc '(openrouter . "google/gemini-pro-1.5")
                                   (plist-get result :mapping)))
                       "gemini-1-5-pro"))))))

(ert-deftest llm-pick-align-test-maps-known-pair-to-one-canonical ()
  (let ((llm-pick-core-default-capability-source 'benchlm))
    (let ((result (llm-pick-align--align '((benchlm . ("claude-3.5-sonnet"))
                                     (openrouter . ("anthropic/claude-3.5-sonnet"))))))
      (ert-info ("Both spellings map onto the normalized anchor ID")
        (should (equal (plist-get result :mapping)
                       '(((benchlm . "claude-3.5-sonnet") . "claude-3-5-sonnet")
                         ((openrouter . "anthropic/claude-3.5-sonnet") . "claude-3-5-sonnet")))))
      (ert-info ("A clean alignment reports no warnings and no standalone IDs")
        (should (equal (plist-get result :warnings) nil))
        (should (equal (plist-get result :standalone) nil))))))

(ert-deftest llm-pick-align-test-anchor-conflict-signals ()
  (ert-info ("Two anchor IDs normalizing alike must stop the alignment")
    (should-error (llm-pick-align--align '((benchlm . ("foo-bar" "foo_bar"))
                                     (openrouter . ("x/foo-bar")))
                                   'benchlm)
                  :type 'llm-pick-align-conflict)))

(ert-deftest llm-pick-align-test-ambiguous-match-signals ()
  ;; Both candidates are the same tokens in a different order, so both are
  ;; genuinely as close to the input and the choice cannot be made.  Two
  ;; candidates of different strength are not ambiguous: a generic name and
  ;; its versioned siblings score apart and stay unmatched, see
  ;; `llm-pick-align-test-a-generic-name-does-not-match-its-versions'.
  (let ((llm-pick-align-match-threshold 0.85)
        (llm-pick-align-match-ambiguity-gap 0.05))
    (ert-info ("Two equally close anchor IDs must not be guessed")
      (should-error (llm-pick-align--align '((benchlm . ("claude-4-sonnet"
                                                   "sonnet-claude-4"))
                                       (openrouter . ("anthropic/claude-sonnet-4")))
                                     'benchlm)
                    :type 'llm-pick-align-ambiguous))))

(ert-deftest llm-pick-align-test-standalone-fallback ()
  (let ((llm-pick-align-match-threshold 0.85)
        (llm-pick-align-on-unmatched 'standalone))
    (let ((result (llm-pick-align--align '((benchlm . ("claude-3.5-sonnet"))
                                     (openrouter . ("some/unknown-model-xyz"))))))
      (ert-info ("An unmatched ID becomes a standalone model")
        (should (equal (plist-get result :standalone)
                       '("some-unknown-model-xyz"))))
      (ert-info ("It is reported as a warning naming source and ID")
        (should (equal (mapcar (lambda (warning)
                                 (list (plist-get warning :source)
                                       (plist-get warning :id)))
                               (plist-get result :warnings))
                       '((openrouter "some/unknown-model-xyz")))))
      (ert-info ("The warning carries the score of the best near miss")
        (should (numberp (plist-get (car (plist-get result :warnings)) :score))))
      (ert-info ("It still gets an entry in the mapping")
        (should (equal (assoc '(openrouter . "some/unknown-model-xyz")
                              (plist-get result :mapping))
                       '((openrouter . "some/unknown-model-xyz") . "some-unknown-model-xyz")))))))

(ert-deftest llm-pick-align-test-unmatched-error-option ()
  (let ((llm-pick-align-match-threshold 0.85)
        (llm-pick-align-on-unmatched 'error))
    (ert-info ("`llm-pick-align-on-unmatched' = error turns a near miss into a failure")
      (should-error (llm-pick-align--align '((benchlm . ("claude-3.5-sonnet"))
                                       (openrouter . ("some/unknown-model-xyz")))
                                     'benchlm)
                    :type 'llm-pick-align-unmatched))))

(ert-deftest llm-pick-align-test-missing-anchor-signals ()
  (ert-info ("Aligning without the anchor source is a programming error")
    (should-error (llm-pick-align--align '((openrouter . ("openai/gpt-4o"))) 'benchlm)
                  :type 'llm-pick-error)))

(ert-deftest llm-pick-align-test-records-last-report ()
  (let ((llm-pick-align--last-report llm-pick-align--last-report))
    (llm-pick-align--align '((benchlm . ("gpt-4o"))) 'benchlm)
    (ert-info ("The last report is kept for the alignment report command")
      (should (equal (plist-get llm-pick-align--last-report :mapping)
                     '(((benchlm . "gpt-4o") . "gpt-4o")))))))

(provide 'llm-pick-align-test)

;;; llm-pick-align-test.el ends here
