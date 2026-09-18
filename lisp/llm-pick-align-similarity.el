;;; llm-pick-align-similarity.el --- How alike two model IDs are -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: Claude
;; URL: https://github.com/OverbearingPearl/llm-pick
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The similarity functions `llm-pick-align' scores normalized IDs with.
;; Each function of `llm-pick-align-similarity-fns' takes two normalized
;; IDs and returns a number in [0, 1], or nil to let the next function
;; try.  When a new naming scheme appears, add one function here.

;;; Code:

(require 'cl-lib)

(defun llm-pick-align-similarity-equal (a b)
  "Return 1.0 when A and B are equal."
  (when (equal a b) 1.0))

(defun llm-pick-align-similarity-coverage (a b)
  "Return how much of the longer of A and B the shorter one accounts for.
The result is in (0, 1].  Two IDs are only as close to each other as the
text they actually share, so a generic name scores high against a name of
its own length and low against every versioned model of its family."
  (/ (float (min (length a) (length b)))
     (max (length a) (length b))))

(defun llm-pick-align-similarity-prefix (a b)
  "Return the score of a prefix match between A and B.
One being a prefix of the other is strong evidence, so the ceiling is
0.95, but the score is the ceiling times the coverage
`llm-pick-align-similarity-coverage' measures: `gpt-5' against `gpt-5-6-sol'
scores 0.43, because the two share five characters out of eleven."
  (when (and (> (length a) 3) (> (length b) 3)
             (or (string-prefix-p a b) (string-prefix-p b a)))
    (* 0.95 (llm-pick-align-similarity-coverage a b))))

(defun llm-pick-align-similarity-substring (a b)
  "Return the score of a substring match between A and B.
One containing the other says less than one starting where the other
starts, so the ceiling is 0.90 rather than 0.95, and the coverage scales
it the same way."
  (when (and (> (length a) 6) (> (length b) 6)
             (or (string-match-p (regexp-quote a) b)
                 (string-match-p (regexp-quote b) a)))
    (* 0.90 (llm-pick-align-similarity-coverage a b))))

(defun llm-pick-align-similarity-token (a b)
  "Return the Jaccard score of the tokens of A and B, scaled by 0.95.
Nil when the two share no token at all.  A zero would end the list of
`llm-pick-align-similarity-fns' on a number that says nothing: every anchor ID
would score the same against A, and the `best match' column of the
alignment report would name an arbitrary one.  Answering nil instead
lets the edit distance score each candidate on its own, still below the
threshold, but a score the reader can rank a long list by."
  (let* ((tokens-a (delete-dups (split-string a "-" t)))
         (tokens-b (delete-dups (split-string b "-" t)))
         (common (length (cl-intersection tokens-a tokens-b :test #'equal))))
    (when (> common 0)
      (* 0.95 (/ (float common)
                 (length (cl-union tokens-a tokens-b :test #'equal)))))))

(defun llm-pick-align-similarity--edit-distance (s1 s2)
  "Return the Levenshtein distance between S1 and S2."
  (let* ((n (length s1))
         (m (length s2))
         (previous (number-sequence 0 m))
         (current (make-list (1+ m) 0)))
    (dotimes (i n)
      (setcar current (1+ i))
      (dotimes (j m)
        (let ((cost (if (eq (aref s1 i) (aref s2 j)) 0 1)))
          (setcar (nthcdr (1+ j) current)
                  (min (1+ (nth j current))
                       (1+ (nth (1+ j) previous))
                       (+ cost (nth j previous))))))
      (setq previous (copy-sequence current)))
    (nth m previous)))

(defun llm-pick-align-similarity--edit-distance-ratio (s1 s2)
  "Return the edit distance of S1 and S2 normalized to [0, 1]."
  (let ((longest (max (length s1) (length s2))))
    (if (= longest 0)
        1.0
      (- 1.0 (/ (float (llm-pick-align-similarity--edit-distance s1 s2)) longest)))))

(defun llm-pick-align-similarity-edit-distance (a b)
  "Return the normalized edit distance similarity of A and B."
  (llm-pick-align-similarity--edit-distance-ratio a b))

(defvar llm-pick-align-similarity-fns
  (list #'llm-pick-align-similarity-equal
        #'llm-pick-align-similarity-prefix
        #'llm-pick-align-similarity-substring
        #'llm-pick-align-similarity-token
        #'llm-pick-align-similarity-edit-distance)
  "Functions that score how similar two normalized IDs are.
Each function takes two normalized IDs and returns a number in [0, 1],
or nil to let the next function try.  The first non-nil result wins, so
keep the sharp functions in front and append refinements at the end.

A sharp function that answers with a low score still ends the list on
purpose.  A prefix or a substring match is decided evidence rather than a
hint, and its coverage is the strength of that evidence, so a broad
scorer must not talk the comparison back up: letting the edit distance
have the last word on `claude-opus-4' against `claude-opus-4-7' and
`claude-opus-4-8' scores both at 0.867 and turns a `no such model' into
an ambiguity nobody can resolve.")

(defun llm-pick-align-similarity-score (a b)
  "Return how similar the normalized IDs A and B are, a number in [0, 1]."
  (or (cl-some (lambda (function) (funcall function a b))
               llm-pick-align-similarity-fns)
      0.0))

(provide 'llm-pick-align-similarity)

;;; llm-pick-align-similarity.el ends here
