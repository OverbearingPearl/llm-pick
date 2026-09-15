;;; llm-pick-fixture-test.el --- Snapshot sources for the test suite -*- lexical-binding: t; -*-

;;; Commentary:

;;
;; The built-in sources read their service only: the illustrative
;; snapshots under a temporary directory the tests write belong to the test suite, not to
;; `llm-pick-source', so the names the suite collects from get a snapshot
;; loader here.
;;
;; The registration runs when this file is loaded, not when a function is
;; called.  `llm-pick-test--reload' reads every *-test.el file of lisp/ on
;; each run, so a run always registers what the files on disk declare --
;; including the run of a session whose `llm-pick-test-run' is still the
;; revision it loaded before this file existed, a file the reload never
;; reads again.
;;
;; Re-registering a name replaces its descriptor and keeps its position,
;; and the :fetcher stays in place, so a test that leaves
;; `llm-pick-source-offline' at nil still reaches the service.

;;; Code:

(require 'llm-pick-source)

(llm-pick-source-register
 'benchlm
 :kind 'capability
 :description "BenchLM capability scores"
 :loader #'llm-pick-source--fixture-loader
 :fixture "benchlm-sample.json"
 :fetcher #'llm-pick-source--benchlm-loader)

(llm-pick-source-register
 'openrouter
 :kind 'price
 :description "OpenRouter pricing"
 :loader #'llm-pick-source--fixture-loader
 :fixture "openrouter-sample.json"
 :fetcher #'llm-pick-source--openrouter-loader)

(provide 'llm-pick-fixture-test)

;;; llm-pick-fixture-test.el ends here
