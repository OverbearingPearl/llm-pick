;;; llm-pick-fixture-test.el --- Snapshot sources for the test suite -*- lexical-binding: t; -*-

;;; Commentary:

;;
;; The built-in sources read their service only, so the suite registers
;; snapshot sources here.  The snapshots are inline JSON strings parsed
;; directly by `llm-pick-source--fixture-loader', so no *.json file is
;; needed.  The registration runs when this file is loaded, not when a
;; function is called, and re-registering a name replaces its descriptor
;; and keeps its position, so a run always registers what the files on
;; disk declare.

;;; Code:

(require 'llm-pick-source)

(llm-pick-source-register
 'benchlm
 :kind 'capability
 :description "BenchLM capability scores"
 :loader #'llm-pick-source--fixture-loader
 :fixture "{\"models\": [{\"id\": \"claude-3.5-sonnet\", \"name\": \"Claude 3.5 Sonnet\", \"provider_ids\": {\"anthropic\": \"claude-3-5-sonnet-20241022\"}, \"scores\": {\"coding\": 88, \"math\": 80}}, {\"id\": \"gpt-4o\", \"name\": \"GPT-4o\", \"provider_ids\": {\"openai\": \"gpt-4o\"}, \"scores\": {\"coding\": 92, \"math\": 90}}, {\"id\": \"gpt-4o-mini\", \"name\": \"GPT-4o mini\", \"provider_ids\": {\"openai\": \"gpt-4o-mini\"}, \"scores\": {\"coding\": 78, \"math\": 70}}, {\"id\": \"gemini-1.5-flash\", \"name\": \"Gemini 1.5 Flash\", \"scores\": {\"coding\": 82, \"math\": 84}}, {\"id\": \"llama-3.1-8b\", \"name\": \"Llama 3.1 8B\", \"scores\": {\"coding\": 65, \"math\": 60}}, {\"id\": \"orphan-model\", \"name\": \"Orphan Model\", \"scores\": {\"coding\": 70, \"math\": 70}}]}"
 :fetcher #'llm-pick-source--benchlm-loader)

(llm-pick-source-register
 'openrouter
 :kind 'price
 :description "OpenRouter pricing"
 :loader #'llm-pick-source--fixture-loader
 :fixture "{\"models\": [{\"id\": \"anthropic/claude-3.5-sonnet\", \"name\": \"Anthropic: Claude 3.5 Sonnet\", \"provider_ids\": {\"openrouter\": \"anthropic/claude-3.5-sonnet\"}, \"pricing\": {\"prompt\": 3.0, \"completion\": 15.0}}, {\"id\": \"openai/gpt-4o\", \"name\": \"OpenAI: GPT-4o\", \"provider_ids\": {\"openrouter\": \"openai/gpt-4o\", \"openai\": \"gpt-4o\"}, \"pricing\": {\"prompt\": 5.0, \"completion\": 15.0}}, {\"id\": \"openai/gpt-4o-mini\", \"name\": \"OpenAI: GPT-4o mini\", \"provider_ids\": {\"openrouter\": \"openai/gpt-4o-mini\"}, \"pricing\": {\"prompt\": 0.15, \"completion\": 0.6}}, {\"id\": \"google/gemini-flash-1.5\", \"name\": \"Google: Gemini Flash 1.5\", \"provider_ids\": {\"openrouter\": \"google/gemini-flash-1.5\"}, \"pricing\": {\"prompt\": 0.075, \"completion\": 0.3}}, {\"id\": \"meta-llama/llama-3.1-8b-instruct\", \"name\": \"Meta: Llama 3.1 8B Instruct\", \"provider_ids\": {\"openrouter\": \"meta-llama/llama-3.1-8b-instruct\"}, \"pricing\": {\"prompt\": 0.05, \"completion\": 0.1}}, {\"id\": \"qwen/qwen-2.5-72b\", \"name\": \"Qwen 2.5 72B\", \"provider_ids\": {\"openrouter\": \"qwen/qwen-2.5-72b\"}, \"pricing\": {\"prompt\": 0.35, \"completion\": 0.4}}]}"
 :fetcher #'llm-pick-source--openrouter-loader)

(provide 'llm-pick-fixture-test)

;;; llm-pick-fixture-test.el ends here
