;;; llm-pick-view-sort-test.el --- Tests for the view sorting -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for `llm-pick-view--sort-key', `llm-pick-view--sort' and
;; `llm-pick-view-sort-toggle': key-to-column mapping, sort direction
;; and the flip on a repeated key.

;;; Code:

(require 'ert)
(require 'llm-pick-view)

(defun llm-pick-view-sort-test--record (canonical scores benchlm-in benchlm-cache benchlm-out or-in or-cache or-out)
  "Build one fixture record named CANONICAL.
SCORES is the alist `llm-pick-core--score' reads.
BENCHLM-IN, BENCHLM-CACHE and BENCHLM-OUT form the BenchLM
price triple; OR-IN, OR-CACHE and OR-OUT form the OpenRouter
one."
  (list :canonical canonical
        :display-name canonical
        :scores scores
        :prices (list (cons 'benchlm
                            (list :in benchlm-in
                                  :cache benchlm-cache
                                  :out benchlm-out))
                      (cons 'openrouter
                            (list :in or-in
                                  :cache or-cache
                                  :out or-out)))))

(defvar llm-pick-view-sort-test--models
  (list
   (llm-pick-view-sort-test--record
    "alpha"
    '(((benchlm . "agentic") . 50) ((benchlm . "coding") . 80))
    1.0 0.5 2.0 3.0 1.5 6.0)
   (llm-pick-view-sort-test--record
    "beta"
    '(((benchlm . "agentic") . 20) ((benchlm . "coding") . 60))
    2.0 1.0 4.0 5.0 2.5 10.0)
   (llm-pick-view-sort-test--record
    "gamma"
    '(((benchlm . "agentic") . 30) ((benchlm . "coding") . 70))
    1.5 0.0 3.0 0.0 2.0 8.0))
  "Fixture records with distinct values in every sortable column.")

(defun llm-pick-view-sort-test--names (records)
  "Return the canonical names of RECORDS in order."
  (mapcar (lambda (r) (llm-pick-core--field r 'name)) records))

;;; Sort key mapping

(ert-deftest llm-pick-view-sort-test-key-plain-field-uses-core-field ()
  (let ((llm-pick-core-default-capability-source 'benchlm) (llm-pick-view--order 'score))
    (should (= (llm-pick-view--sort-key (nth 0 llm-pick-view-sort-test--models))
               (llm-pick-core--field (nth 0 llm-pick-view-sort-test--models)
                                     'score)))))

(ert-deftest llm-pick-view-sort-test-key-category-uses-core-score ()
  (let ((llm-pick-core-default-capability-source 'benchlm) (llm-pick-view--order '(benchlm . "coding")))
    (should (= (llm-pick-view--sort-key (nth 1 llm-pick-view-sort-test--models))
               60))))

(ert-deftest llm-pick-view-sort-test-key-price-part-uses-core-price ()
  (let ((llm-pick-core-default-capability-source 'benchlm) (llm-pick-view--order '(openrouter . out)))
    (should (= (llm-pick-view--sort-key (nth 2 llm-pick-view-sort-test--models))
               8.0)))
  (let ((llm-pick-core-default-capability-source 'benchlm) (llm-pick-view--order '(benchlm . in)))
    (should (= (llm-pick-view--sort-key (nth 0 llm-pick-view-sort-test--models))
               1.0))))

(ert-deftest llm-pick-view-sort-test-missing-key-ranks-last ()
  (let ((llm-pick-core-default-capability-source 'benchlm) (llm-pick-view--order 'score))
    (should (= (llm-pick-view--sort-key '(:canonical "no/scores")) -1))))

;;; Sort direction and flip

(ert-deftest llm-pick-view-sort-test-numeric-default-is-descending ()
  (let ((llm-pick-core-default-capability-source 'benchlm) (llm-pick-view--order '(benchlm . "agentic"))
        (llm-pick-view--reverse nil))
    (should (equal (llm-pick-view-sort-test--names
                    (llm-pick-view--sort llm-pick-view-sort-test--models))
                   '("alpha" "gamma" "beta")))))

(ert-deftest llm-pick-view-sort-test-flip-reverses-numeric-order ()
  (let ((llm-pick-core-default-capability-source 'benchlm) (llm-pick-view--order '(benchlm . "agentic"))
        (llm-pick-view--reverse nil))
    (let ((down (llm-pick-view-sort-test--names
                 (llm-pick-view--sort llm-pick-view-sort-test--models))))
      (setq llm-pick-view--reverse t)
      (should (equal (llm-pick-view-sort-test--names
                      (llm-pick-view--sort llm-pick-view-sort-test--models))
                     (reverse down))))))

(ert-deftest llm-pick-view-sort-test-flip-reverses-name-order ()
  (let ((llm-pick-core-default-capability-source 'benchlm) (llm-pick-view--order 'name)
        (llm-pick-view--reverse nil))
    (let ((up (llm-pick-view-sort-test--names
               (llm-pick-view--sort llm-pick-view-sort-test--models))))
      (should (equal up '("alpha" "beta" "gamma")))
      (setq llm-pick-view--reverse t)
      (should (equal (llm-pick-view-sort-test--names
                      (llm-pick-view--sort llm-pick-view-sort-test--models))
                     '("gamma" "beta" "alpha"))))))

;;; Toggle state machine: first press sets order, second flips

(ert-deftest llm-pick-view-sort-test-toggle-first-press-sets-order ()
  (let ((llm-pick-core-default-capability-source 'benchlm) (llm-pick-view--order nil)
        (llm-pick-view--reverse nil))
    (cl-letf (((symbol-function 'llm-pick-view-main) #'ignore))
      (llm-pick-view-sort-toggle '(benchlm . "math"))
      (should (equal llm-pick-view--order '(benchlm . "math")))
      (should (null llm-pick-view--reverse)))))

(ert-deftest llm-pick-view-sort-test-toggle-second-press-flips ()
  (let ((llm-pick-core-default-capability-source 'benchlm) (llm-pick-view--order '(benchlm . "math"))
        (llm-pick-view--reverse nil))
    (cl-letf (((symbol-function 'llm-pick-view-main) #'ignore))
      (llm-pick-view-sort-toggle '(benchlm . "math"))
      (should llm-pick-view--reverse)
      (llm-pick-view-sort-toggle '(benchlm . "math"))
      (should (null llm-pick-view--reverse)))))

(ert-deftest llm-pick-view-sort-test-toggle-different-column-resets-flip ()
  (let ((llm-pick-core-default-capability-source 'benchlm) (llm-pick-view--order 'score)
        (llm-pick-view--reverse t))
    (cl-letf (((symbol-function 'llm-pick-view-main) #'ignore))
      (llm-pick-view-sort-toggle '(openrouter . out))
      (should (equal llm-pick-view--order '(openrouter . out)))
      (should (null llm-pick-view--reverse)))))

;;; Keymap wiring: every sort key dispatches the right order value

(ert-deftest llm-pick-view-sort-test-keymap-keys-map-to-their-columns ()
  (let ((expected
         '(("N" . name)
           ("S" . score)
           ("A" . (benchlm . "agentic"))
           ("C" . (benchlm . "coding"))
           ("R" . (benchlm . "reasoning"))
           ("m" . (benchlm . "multimodalGrounded"))
           ("K" . (benchlm . "knowledge"))
           ("l" . (benchlm . "multilingual"))
           ("s" . (benchlm . "instructionFollowing"))
           ("a" . (benchlm . "math"))
           ("i" . (openrouter . "intelligence"))
           ("o" . (openrouter . "coding"))
           ("e" . (openrouter . "agentic"))
           ("I" . (benchlm . in))
           ("c" . (benchlm . cache))
           ("O" . (benchlm . out))
           ("u" . (openrouter . in))
           ("h" . (openrouter . cache))
           ("t" . (openrouter . out)))))
    (dolist (spec expected)
      (let ((binding (lookup-key llm-pick-view-mode-map (kbd (car spec)))))
        (should (functionp binding))
        (let ((llm-pick-core-default-capability-source 'benchlm) (llm-pick-view--order nil)
              (llm-pick-view--reverse nil))
          (cl-letf (((symbol-function 'llm-pick-view-main) #'ignore))
            (funcall binding)
            (should (equal llm-pick-view--order (cdr spec)))))))))

(provide 'llm-pick-view-sort-test)

;;; llm-pick-view-sort-test.el ends here
