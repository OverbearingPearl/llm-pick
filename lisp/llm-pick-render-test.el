;;; llm-pick-render-test.el --- Tests for llm-pick-render-report-text -*- lexical-binding: t; -*-

;;; Commentary:

;;
;; Rendering is pure, so the expectations are literal strings: what the
;; user sees is what these tests compare against.  The tests bind the
;; source options they depend on, so they do not depend on the
;; configuration of the running Emacs.

;;; Code:

(require 'ert)
(require 'llm-pick-render-report)

(defconst llm-pick-render-test--models
  (list (llm-pick-core--make-record "gpt-4o"
                               :scores '((benchlm . 92))
                               :prices '((openrouter :in 5.0 :out 15.0)))
        (llm-pick-core--make-record "weak"
                               :scores '((benchlm . 60))
                               :prices '((openrouter :in 0.1 :out 0.5))))
  "Records shared by the rendering tests.")

(ert-deftest llm-pick-render-test-bar-scales-to-the-maximum ()
  (let ((llm-pick-render-report-default-bar-width 10))
    (ert-info ("Half the maximum fills half the bar")
      (should (equal (llm-pick-render-report--bar 50 100)
                     (concat (make-string 5 ?█) (make-string 5 ?░)))))
    (ert-info ("A model without a score draws an empty bar")
      (should (equal (llm-pick-render-report--bar nil 100)
                     (make-string 10 ?░))))
    (ert-info ("The bar never overflows its width")
      (should (equal (llm-pick-render-report--bar 120 100)
                     (make-string 10 ?█))))))

(ert-deftest llm-pick-render-test-table-aligns-columns ()
  (let ((llm-pick-core-default-capability-source 'benchlm)
        (llm-pick-core-default-price-source 'openrouter))
    (ert-info ("Text is left aligned, numbers right aligned")
      (should (equal (llm-pick-render-report-table llm-pick-render-test--models
                                            '(name score or-out))
                     (concat "Model   Score  $/M out\n"
                             "gpt-4o     92       15\n"
                             "weak       60      0.5"))))))

(ert-deftest llm-pick-render-test-missing-value-is-marked ()
  (let ((llm-pick-core-default-capability-source 'benchlm)
        (llm-pick-core-default-price-source 'openrouter))
    (let ((text (llm-pick-render-report-table (list (llm-pick-core--make-record "bare"))
                                       '(name or-out))))
      (ert-info ("A model without a price is marked, not left blank")
        (should (string-match-p "—" text)))
      (ert-info ("Every line is padded to the same width")
        (let ((lines (split-string text "\n")))
          (should (= (length (nth 0 lines)) (length (nth 1 lines)))))))))

(ert-deftest llm-pick-render-test-explicit-source-column ()
  (let ((llm-pick-core-default-capability-source 'benchlm))
    (ert-info ("The header names the source and the cell its score")
      (should (equal (llm-pick-render-report-table
                      (list (llm-pick-core--make-record
                             "m" :scores '((benchlm . 88) (other . 70))))
                      '((score other)))
                     (concat "Score other\n"
                             "         70"))))))

(ert-deftest llm-pick-render-test-csv-quotes-fields ()
  (ert-info ("A field with a separator or a quote is quoted and doubled")
    (should (equal (llm-pick-render-report-csv
                    (list (llm-pick-core--make-record "a,b\"c"))
                    '(name))
                   "Model\n\"a,b\"\"c\"")))
  (ert-info ("A plain field stays unquoted")
    (should (equal (llm-pick-render-report-csv (list (llm-pick-core--make-record "plain"))
                                        '(name))
                   "Model\nplain"))))

(ert-deftest llm-pick-render-test-unknown-format-signals ()
  (ert-info ("A typo in a format must not be answered with a table")
    (should-error (llm-pick-render-report-text '() '(name) 'xml)
                  :type 'llm-pick-error)))

(ert-deftest llm-pick-render-test-frontier-shows-the-gain-per-step ()
  (let ((llm-pick-core-default-capability-source 'benchlm)
        (llm-pick-core-default-price-source 'openrouter)
        (llm-pick-render-report-default-bar-width 10)
        (llm-pick-render-report-marginal-threshold 2.0))
    (ert-info ("Cheapest first; the step up buys 32/14.5 points per dollar")
      (should (equal (llm-pick-render-report-frontier llm-pick-render-test--models)
                     (concat "Model   Capability  Score  $/M out  Gain/$\n"
                             "weak    " (make-string 7 ?█) "░░░"
                             "     60      0.5       —\n"
                             "gpt-4o  " (make-string 10 ?█)
                             "     92       15    +2.2"))))))

(ert-deftest llm-pick-render-test-frontier-flags-a-dear-step ()
  (let ((llm-pick-core-default-capability-source 'benchlm)
        (llm-pick-core-default-price-source 'openrouter)
        (llm-pick-render-report-default-bar-width 10)
        (llm-pick-render-report-marginal-threshold 5.0))
    (ert-info ("A step below the threshold is marked and explained")
      (let ((text (llm-pick-render-report-frontier llm-pick-render-test--models)))
        (should (string-match-p "\\+2\\.2 !" text))
        (should (string-match-p "less than 5.0 points per dollar" text))))))

(ert-deftest llm-pick-render-test-gap-column-is-a-percentage ()
  (let ((llm-pick-core-default-capability-source 'benchlm)
        (llm-pick-core-default-price-source 'openrouter)
        (llm-pick-core-secondary-price-source 'bedrock))
    (ert-info ("$12 against $15 shows as a 25% gap under its own header")
      (let ((text (llm-pick-render-report-table
                   (list (llm-pick-core--make-record
                          "m" :prices '((openrouter :out 15.0)
                                        (bedrock :out 12.0))))
                   '(name gap))))
        (should (string-match-p "Gap" text))
        (should (string-match-p "25%" text))))))

(ert-deftest llm-pick-render-test-ladder-buckets-by-price ()
  (let ((llm-pick-core-default-capability-source 'benchlm)
        (llm-pick-core-default-price-source 'openrouter)
        (llm-pick-render-report-default-bar-width 10))
    (ert-info ("Each bucket shows its most capable model, the open one last")
      (should (equal (llm-pick-render-report-ladder llm-pick-render-test--models
                                             '(1.0 nil))
                     (concat "Up to $/M out  Model   Score  $/M out\n"
                             "            1  weak       60      0.5\n"
                             "         open  gpt-4o     92       15"))))))

(defun llm-pick-render-test--entry (source id norm canonical kind &optional score best)
  "Return an alignment entry as `llm-pick-align--align' builds it.
SOURCE is the source symbol; ID is the ID that source spells.
NORM is the normalized form; CANONICAL is the canonical ID it was taken for.
KIND is the alignment kind; SCORE and BEST are optional similarity scores."
  (list :source source :id id :norm norm :canonical canonical
        :kind kind :score score :best best))

(defconst llm-pick-render-test--alignment
  (list :mapping '(((benchlm . "benchlm-one") . "one")
                   ((benchlm . "benchlm-two") . "two")
                   ((openrouter . "vendor/one") . "one")
                   ((openrouter . "vendor/two-2024") . "two")
                   ((openrouter . "vendor/three") . "three")
                   ((openrouter . "vendor/far") . "far"))
        :entries (list (llm-pick-render-test--entry
                        'benchlm "benchlm-one" "one" "one" 'anchor)
                       (llm-pick-render-test--entry
                        'benchlm "benchlm-two" "two" "two" 'anchor)
                       ;; Normalized to the anchor's model: no decision.
                       (llm-pick-render-test--entry
                        'openrouter "vendor/one" "one" "one" 'exact)
                       ;; A similarity score decided this one.
                       (llm-pick-render-test--entry
                        'openrouter "vendor/two-2024" "two-2024" "two"
                        'fuzzy 0.900)
                       ;; Several sources spelled this one alike.
                       (llm-pick-render-test--entry
                        'openrouter "vendor/three" "three" "three"
                        'agreed 0.800)
                       ;; Nothing matched, and close enough to read.
                       (llm-pick-render-test--entry
                        'openrouter "vendor/four" "four" "four"
                        'unmatched 0.820 "two")
                       ;; Nothing matched, and nowhere near.
                       (llm-pick-render-test--entry
                        'openrouter "vendor/far" "far" "far"
                        'unmatched 0.100 "one")))
  "An alignment report that exercises every section of the renderer.")

(ert-deftest llm-pick-render-test-alignment-counts-what-it-does-not-list ()
  (let ((llm-pick-align-match-threshold 0.85))
    (let ((text (llm-pick-render-report-alignment llm-pick-render-test--alignment)))
      (ert-info ("The counts say how much was merged and how much was not")
        (should (string-match-p "6 IDs from 2 sources onto 4 canonical models"
                                text))
        (should (string-match-p "2 canonical models named by more than one source"
                                text))
        (should (string-match-p "2 canonical models named by one source only"
                                text)))
      (ert-info ("An ID that decided nothing is counted, not listed")
        (should (string-match-p "1 ID normalizes to the anchor's model already"
                                text))
        (should-not (string-match-p "vendor/one" text))))))

(ert-deftest llm-pick-render-test-alignment-names-source-id-and-normal-form ()
  ;; The point of the report: every decision shows the source, the ID as
  ;; that source spells it, what the normalizer made of it and the model
  ;; it was taken for.
  (let ((llm-pick-align-match-threshold 0.85))
    (let ((text (llm-pick-render-report-alignment llm-pick-render-test--alignment)))
      (ert-info ("A merge a score decided names all four")
        (should (string-match-p
                 "openrouter  vendor/two-2024\\s-+two-2024\\s-+two\\s-+0.900"
                 text)))
      (ert-info ("A merge several sources agreed on names them")
        (should (string-match-p
                 "openrouter  vendor/three\\s-+three\\s-+three"
                 text)))
      (ert-info ("A near miss names the model it came closest to")
        (should (string-match-p
                 "openrouter  vendor/four\\s-+four\\s-+two\\s-+0.820"
                 text))))))

(ert-deftest llm-pick-render-test-alignment-hides-the-far-misses ()
  ;; A live collection leaves hundreds of IDs the anchor never scored.
  ;; Listing them buried the handful whose score sits just under the
  ;; threshold, which are the only ones a reader can act on.
  (let ((llm-pick-align-match-threshold 0.85))
    (let ((text (llm-pick-render-report-alignment llm-pick-render-test--alignment)))
      (ert-info ("The near miss is listed, the far one is only counted")
        (should (string-match-p "vendor/four" text))
        (should-not (string-match-p "vendor/far" text))
        (should (string-match-p "1 ID further away" text)))
      (ert-info ("The totals count both: the section names every unmatched ID")
        (should (string-match-p "Matched no model of the anchor source (2)"
                                text))
        (should (string-match-p "1 ID within 0.10 of a match" text))))))

(ert-deftest llm-pick-render-test-alignment-can-list-the-undecided-ids ()
  ;; `C-u M-x llm-pick-align-report' lists them, because a normalization
  ;; rule that drops too much is exactly what lands an ID on a model that
  ;; is spelled like it and is not it.
  (let ((llm-pick-align-match-threshold 0.85))
    (let ((text (llm-pick-render-report-alignment llm-pick-render-test--alignment t)))
      (ert-info ("The undecided ID is listed with its normalized form")
        (should (string-match-p "vendor/one\\s-+one\\s-+one" text)))
      (ert-info ("Every section is still there")
        (should (string-match-p "vendor/two-2024" text))
        (should (string-match-p "vendor/four" text))))))

(ert-deftest llm-pick-render-test-alignment-can-list-every-unmatched-id ()
  ;; With ALL non-nil even entries far below the near-miss band are
  ;; listed, so nothing silently disappears from the report.
  (let ((entry (list :kind 'unmatched
                     :id "vendor/far"
                     :norm "vendor far far"
                     :best "vendor/somewhere-else"
                     :score 0.3)))
    (let ((report (list :entries (list entry)
                        :mapping (list (cons (cons "openrouter" "vendor/far") "vendor/far")))))
      (ert-info ("With ALL t the far-below-band entry is listed")
        (let ((text (llm-pick-render-report-alignment report t)))
          (should (string-match-p "vendor far far" text))))
      (ert-info ("With ALL nil it stays hidden, as today")
        (let ((text (llm-pick-render-report-alignment report nil)))
          (should-not (string-match-p "vendor far far" text)))))))

(ert-deftest llm-pick-render-test-alignment-leaves-the-report-alone ()
  (let ((llm-pick-align-match-threshold 0.85)
        (entries (copy-sequence (plist-get llm-pick-render-test--alignment
                                           :entries))))
    (llm-pick-render-report-alignment llm-pick-render-test--alignment t)
    (ert-info ("Rendering sorts copies, so the caller's list keeps its order")
      (should (equal (plist-get llm-pick-render-test--alignment :entries)
                     entries)))))

(ert-deftest llm-pick-render-test-problems-are-grouped-by-kind ()
  (ert-info ("Each kind gets its own heading and count")
    (let ((text (llm-pick-render-report-problems
                 '((:type llm-pick-align-conflict :description "first conflict")
                   (:type llm-pick-align-conflict :description "second conflict")
                   (:type llm-pick-align-unmatched :description "one miss")))))
      (should (string-match-p "3 problems in this run" text))
      (should (string-match-p (regexp-quote "ID alignment conflict (2)") text))
      (should (string-match-p (regexp-quote "ID without a match (1)") text))
      (should (string-match-p "second conflict" text))
      (ert-info ("A description keeps its own line breaks, indented")
        (should (string-match-p "^  first conflict" text)))))
  (ert-info ("A run without a problem says so instead of showing nothing")
    (should (string-match-p "No problem" (llm-pick-render-report-problems nil)))))

(ert-deftest llm-pick-render-test-benchmark-groups-by-output-price-ratio ()
  (let* ((llm-pick-core-default-capability-source 'benchlm)
         (llm-pick-core-default-price-source 'openrouter)
         (base (llm-pick-core--make-record
                "base"
                :scores '((benchlm . 80))
                :prices '((openrouter :out 1.0))))
         (near (llm-pick-core--make-record
                "near"
                :scores '((benchlm . 85))
                :prices '((openrouter :out 1.2))))
         (dear (llm-pick-core--make-record
                "dear"
                :scores '((benchlm . 90))
                :prices '((openrouter :out 4.0))))
         (report (llm-pick-render-report-benchmark
                  (list base near dear)
                  base
                  '(0.5 2.0 nil))))
    (ert-info ("Headline names the baseline")
      (should (string-match-p "base" report)))
    (ert-info ("The near band title appears")
      (should (string-match-p "0\\.50x to 2\\.00x" report)))
    (ert-info ("The dear band title appears")
      (should (string-match-p "2\\.00x or more" report)))
    (ert-info ("The dear model's delta reads +10.0")
      (should (string-match-p "\\+10\\.0" report)))))

(provide 'llm-pick-render-test)

;;; llm-pick-render-test.el ends here
