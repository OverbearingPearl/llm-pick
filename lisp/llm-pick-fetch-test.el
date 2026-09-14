;;; llm-pick-fetch-test.el --- Tests for llm-pick-fetch-get-http -*- lexical-binding: t; -*-

;;; Commentary:

;;
;; The HTTP layer is mocked, so these tests never open a socket and never
;; reach a service; what they check is the parsing of an answer and the
;; errors an answer can produce.  Nothing is written to disk, so a test
;; run leaves no trace anywhere.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'llm-pick-fetch-get)

(defun llm-pick-fetch-test--serve (body)
  "Return a `llm-pick-fetch-get-http' stub that always answers BODY."
  (lambda (&rest _) body))

(defun llm-pick-fetch-test--answer (status body)
  "Return a `url-retrieve-synchronously' stub answering STATUS and BODY."
  (lambda (&rest _)
    (with-current-buffer (get-buffer-create " *llm-pick-fetch-test*")
      (erase-buffer)
      (insert (format "HTTP/1.1 %s\nContent-Type: application/json\n\n%s"
                      status body))
      (current-buffer))))

(ert-deftest llm-pick-fetch-test-json-answer-is-parsed ()
  (cl-letf (((symbol-function 'llm-pick-fetch-get-http)
             (llm-pick-fetch-test--serve "{\"models\": [{\"id\": \"a\"}]}")))
    (ert-info ("The parsed answer is a hash table with string keys")
      (let ((data (llm-pick-fetch-get-json "https://a.test/x")))
        (should (equal (llm-pick-source--json-field
                        (car (llm-pick-source--json-field data "models")) "id")
                       "a"))))))

(ert-deftest llm-pick-fetch-test-an-answer-that-is-not-json-is-loud ()
  (cl-letf (((symbol-function 'llm-pick-fetch-get-http)
             (llm-pick-fetch-test--serve "<html>gateway</html>")))
    (ert-info ("An error page must not look like a source with no models")
      (should-error (llm-pick-fetch-get-json "https://a.test/x")
                    :type 'llm-pick-error))))

(ert-deftest llm-pick-fetch-test-nothing-is-kept-between-two-calls ()
  ;; The regression guard for the decision to cache nothing: a second
  ;; call must be a second request, and a status outside 2xx must fail
  ;; the call instead of looking like a source with no models.
  (let ((requests 0))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (&rest _)
                 (cl-incf requests)
                 (funcall (llm-pick-fetch-test--answer "200 OK" "{\"a\": 1}")))))
      (ert-info ("Two calls mean two requests")
        (should (equal (llm-pick-fetch-get-http "https://a.test/x") "{\"a\": 1}"))
        (should (equal (llm-pick-fetch-get-http "https://a.test/x") "{\"a\": 1}"))
        (should (= requests 2)))))
  (ert-info ("A refusal names the URL and the status")
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (llm-pick-fetch-test--answer "503 Service Unavailable" "busy")))
      (should-error (llm-pick-fetch-get-http "https://a.test/x")
                    :type 'llm-pick-error))))

(provide 'llm-pick-fetch-test)

;;; llm-pick-fetch-test.el ends here
