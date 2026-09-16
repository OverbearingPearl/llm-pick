;;; llm-pick-fetch-get.el --- Fetch source answers over the network -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: madachuan <madachuan.noreply.github.com>
;; Assisted-by: Claude
;; URL: https://github.com/madachuan/llm-pick
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;
;; A source has two ways to get its data: the offline snapshot it ships
;; with, and the service of the project itself.  This module owns the
;; second one, so that `llm-pick-source' only has to say which function
;; belongs to which source.
;;
;; Nothing is kept between two fetches: every collection asks the
;; services again, so what the user reads is what the services say right
;; now.  There is no cache to go stale, to age out, to invalidate, or to
;; ask for a coding system.  The price of that choice is latency, and it
;; is bounded: one request per source, one collection per command.
;;
;; Nothing here opens a socket unless a caller asks for it, and the test
;; suite never asks: it binds `llm-pick-source-offline' to t and drives the
;; source loaders through the snapshots instead.

;;; Code:

(require 'cl-lib)
(require 'url)
(require 'llm-pick-core)

(defconst llm-pick-fetch-get--benchlm-url
  "https://benchlm.ai/api/data/leaderboard"
  "Endpoint of the BenchLM leaderboard.")

(defconst llm-pick-fetch-get--openrouter-url
  "https://openrouter.ai/api/v1/models"
  "Endpoint of the OpenRouter model list.")

(defconst llm-pick-fetch-get--openrouter-benchmarks-url
  "https://openrouter.ai/api/v1/benchmarks"
  "Endpoint of the OpenRouter benchmarks endpoint.

It is the source of the Artificial Analysis capability indices.")

(defconst llm-pick-fetch-get--openrouter-benchmarks-url
  "https://openrouter.ai/api/v1/benchmarks"
  "Endpoint of the OpenRouter benchmarks.")

(defun llm-pick-fetch-get--openrouter-auth ()
  "Return the bearer token for openrouter.ai, or nil if unavailable.
Try, in order: the variable `llm-pick-source-openrouter-api-key',
the OPENROUTER_API_KEY environment variable, and finally
`auth-source-search' with :host \"openrouter.ai\"."
  (cond
   ((and (boundp 'llm-pick-source-openrouter-api-key)
         llm-pick-source-openrouter-api-key)
    llm-pick-source-openrouter-api-key)
   ((getenv "OPENROUTER_API_KEY"))
   (t
    (require 'auth-source)
    (let ((entry (car (auth-source-search
                       :host "openrouter.ai"
                       :require '(secret)
                       :create nil))))
      (when entry
        (let ((secret (plist-get entry :secret)))
          (when secret
            (setq secret (if (functionp secret) (funcall secret) secret))
            (if (multibyte-string-p secret)
                secret
              (decode-coding-string secret 'utf-8)))))))))

;;; HTTP

(defun llm-pick-fetch-get--status (buffer url)
  "Return the status code of the response in BUFFER, from URL.
Signal `llm-pick-error' when there is no status line."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (unless (looking-at "HTTP/[0-9.]+ +\\([0-9]+\\)")
        (signal 'llm-pick-error
                (list (format "%s answered with no status line" url))))
      (string-to-number (match-string 1)))))

(defun llm-pick-fetch-get--body (buffer url)
  "Return the body of the response in BUFFER, from URL."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (unless (search-forward "\n\n" nil t)
        (signal 'llm-pick-error
                (list (format "%s answered with no body" url))))
      (buffer-substring-no-properties (point) (point-max)))))

(defun llm-pick-fetch-get-http (url &optional headers)
  "Return the body of URL as a string, one GET per call.
HEADERS is an alist of extra request headers.  Nothing is kept between
two calls, so the caller reads what the service says right now.

Signal `llm-pick-error' when the request cannot be made, when there is no
response, or when the status is not 2xx; the message carries the URL, the
status and the first line of the body.  Nothing is swallowed: a service
that refuses the request must not look like a source that had nothing to
say."
  (let* ((url-request-extra-headers headers)
         (url-request-method "GET")
         (buffer (condition-case err
                     (url-retrieve-synchronously url t t)
                   ;; The boundary of the url library: turn whatever it
                   ;; signals into the one error a caller of this module
                   ;; catches, and put the URL in the message.
                   (error
                    (signal 'llm-pick-error
                            (list (format "Cannot reach %s: %s"
                                          url (error-message-string err))))))))
    (unless buffer
      (signal 'llm-pick-error (list (format "No response from %s" url))))
    (unwind-protect
        (let ((status (llm-pick-fetch-get--status buffer url))
              (body (llm-pick-fetch-get--body buffer url)))
          (unless (and (>= status 200) (< status 300))
            (signal 'llm-pick-error
                    (list (format "%s answered %d: %s"
                                  url status
                                  (car (split-string body "\n" t))))))
          body)
      (kill-buffer buffer))))

;;; Entry points

(defun llm-pick-fetch-get-json (url &optional headers)
  "Return the JSON body of URL parsed, see `llm-pick-fetch-get-http'.
HEADERS is an alist of extra request headers, passed through to
`llm-pick-fetch-get-http'."
  (llm-pick-core--parse-json (llm-pick-fetch-get-http url headers)))

(provide 'llm-pick-fetch-get)

;;; llm-pick-fetch-get.el ends here
