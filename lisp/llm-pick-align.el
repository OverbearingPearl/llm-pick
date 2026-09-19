;;; llm-pick-align.el --- Fuzzy ID alignment across sources -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: Claude
;; URL: https://github.com/OverbearingPearl/llm-pick
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;
;; Every source spells model IDs differently.  Instead of maintaining a
;; translation table by hand, llm-pick normalizes IDs with
;; `llm-pick-normalize-rules' and scores the remaining candidates with
;; `llm-pick-similarity-fns'.  When a new naming scheme appears, add one
;; rule or one similarity function; both are user options.
;;
;; The canonical ID of a model is the normalized form of its anchor
;; source ID, so the same model from different sources collapses onto a
;; single key.  Two anchor IDs that normalize to the same string are a
;; hard error: silently merging two different models is worse than
;; stopping and asking for a better rule.

;;; Code:

(require 'cl-lib)
(require 'llm-pick-core)
(require 'llm-pick-align-similarity)

;; The user options live in llm-pick.el, which requires this module.
;; Forward declaration for the user option `llm-pick-align-match-threshold'
;; (defined in llm-pick.el; renamed there in a later sync).
(defvar llm-pick-align-match-threshold)
(defvar llm-pick-align-match-ambiguity-gap)
(defvar llm-pick-align-on-unmatched)
(defvar llm-pick-align-match-consensus-sources)
(defvar llm-pick-align-match-consensus-band)

;;; Errors

(define-error 'llm-pick-align-conflict "ID alignment conflict" 'llm-pick-error)
(define-error 'llm-pick-align-ambiguous "ID alignment ambiguity" 'llm-pick-error)
(define-error 'llm-pick-align-unmatched "ID without a match" 'llm-pick-error)

;;; Normalization

(defcustom llm-pick-normalize-rules
  '(;; Whitespace first: the vendor rules below match on the segment
    ;; before the slash, and the leaderboard writes it with spaces.
    ("[ \t]+" . "-")
    ;; A vendor that spells its own name twice keeps one copy, so that
    ;; `minimax/minimax-m2' and `Minimax/Minimax M2.7' meet at
    ;; `minimax-m2' and `minimax-m2-7'.
    ("\\`\\([^/]+\\)/\\(?:\\1/\\)*\\1" . "\\1")
    ;; The vendors both catalogues name are dropped, whichever way they
    ;; spell them: the leaderboard writes a brand (`Alibaba/Qwen3.8 Max',
    ;; `xAI/Grok 4.6') and the price catalogue a slug
    ;; (`qwen/qwen3.8-max-0902', `x-ai/grok-4.6'), and what identifies the
    ;; model is its name.  Both spellings have to be here, or the pair
    ;; keeps a vendor segment on one side and never meets.
    ("\\`\\(?:openai\\|anthropic\\|google\\|meta-llama\\|mistralai\\|deepseek\\|cohere\\|amazon\\|azure\\|qwen\\|alibaba\\|x-ai\\|xai\\|moonshotai\\|moonshot-ai\\|thinkingmachines\\|thinking-machines-lab\\)/" . "")
    ;; A bracketed qualifier is rewritten into its hyphen form before
    ;; the suffix and punctuation rules: the two catalogues spell these
    ;; qualifiers differently (benchlm as `-(high)' or `-[beta]',
    ;; openrouter as `-high' or `-beta'), so `model-(high)' and
    ;; `model-high' both land on `model-high', and `grok-3-[beta]' lands
    ;; on `grok-3-beta', which the suffix rule below then strips.  The
    ;; word list covers high, low, medium, preview, reasoning, thinking,
    ;; adaptive, mini and beta.  Rewritten once, `model-high' no longer
    ;; matches and the rule stays idempotent.
    ("-?(\\(high\\|low\\|medium\\|preview\\|reasoning\\|thinking\\|adaptive\\|mini\\|beta\\))" . "-\\1")
    ("-?\\[\\(high\\|low\\|medium\\|preview\\|reasoning\\|thinking\\|adaptive\\|mini\\|beta\\)\\]" . "-\\1")
    (":.*\\'" . "")
    ("\\(?:-instruct\\|-it\\|-v\\|-chat\\|-base\\|-beta\\|-free\\|-latest\\)\\'" . "")
    ("-[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\'" . "")
    ("-[0-9]\\{4,8\\}\\'" . "")
    ("[:/.]" . "-")
    ;; A family word the catalogue spells twice is collapsed to one
    ;; copy: `ibm-granite/granite-4.0-h-micro' keeps one `granite' and
    ;; meets the anchor's `ibm-granite-...' spelling, and
    ;; `bytedance-seed/seed-2.0-lite' keeps one `seed'.  The inserted
    ;; form ends in a hyphen before a different word, so it cannot
    ;; rematch and the rule stays idempotent.
    ("\\([a-z][a-z0-9]*\\)-\\1-" . "\\1-")
    ("_+" . "-")
    ("-+" . "-")
    ("\\`-\\|-\\'" . ""))
  "Rules that normalize a source ID into a canonical ID.
Each rule is a regexp and a replacement, applied in order by
`llm-pick-align--normalize', which has to stay idempotent.

Both sources put a vendor in front of the model name, and they spell it
differently: the leaderboard writes a brand (`Alibaba/Qwen3.8 Max',
`xAI/Grok 4.6', `Moonshot AI/Kimi K3') while the price catalogue writes
a slug (`qwen/qwen3.8-max-0902', `x-ai/grok-4.6',
`moonshotai/kimi-k3').  Without a rule the vendor is part of the ID and
the two never meet: `qwen/qwen3.8-max-0902' scored 0.521 against
`Alibaba/Qwen3.8 Max' and stayed unmatched.

The rule that fixes that drops the vendor, and it lists the brand and the
slug side by side so that both sides lose it and the pair meets on the
model name: `qwen3-8-max', `grok-4-6', `kimi-k3'.  Rewriting one spelling
into the other would work for the pair as well, but it would also rename
every model only one source lists — `qwen/qwen-2.5-72b' would become
`alibaba-qwen-2-5-72b' — and the question an alignment answers is what
model this is, not who sells it.

Whitespace becomes a dash, so a source that names a model by its display
name (`Claude 3.5 Sonnet') meets the ID another source spells
`claude-3.5-sonnet'.  A vendor that spells its own name twice keeps one
copy, which is what keeps `minimax-minimax-m2' from covering 18 of the 20
characters of both `M2.5' and `M2.7' and tying them.  A family word the
catalogue spells twice (`ibm-granite/granite-4.0-h-micro', which the
punctuation rule turns into `ibm-granite-granite-4.0-h-micro') is
collapsed to one copy the same way, so the pair meets on
`ibm-granite-4.0-h-micro', and `bytedance-seed/seed-2.0-lite' keeps one
`seed'.

The two catalogues also spell bracketed qualifiers differently:
benchlm writes `-(high)' or `-[beta]' where openrouter writes `-high' or
`-beta', so two rules rewrite the parenthesized and square-bracket forms
into the hyphen form and both sides land on the same string.  The word
list covers `medium' as well as high, low, preview, reasoning, thinking,
adaptive, mini and beta.  The rules match the brackets and never a bare
`-word-', because matching `-mini-' would eat the hyphen of
`gpt-4o-mini' and split that ID from the one openrouter spells with no
qualifier.  They run before the suffix-stripping rules, because the
rewritten `-high' and `-beta' are themselves suffixes those rules
understand.

A dot inside a version is the punctuation rule's job: `qwen3.8-max'
becomes `qwen3-8-max' and `minimax-m2.7' stays `minimax-m2-7', with no
hyphen inserted before the version digit.

Release suffixes come before the release dates, so that `model:beta' and
`model' meet on the same canonical ID.  Only a suffix that names the same
model under another spelling belongs in that rule.  `-preview' is not one
of them: a catalogue that lists `Tencent/Hy3' and `Tencent/Hy3 Preview'
offers two models, and merging them would hide one.  A version is not one
of them either: `DeepSeek/DeepSeek V3', `V3.1' and `V3.2' are three
releases, and a rule that drops the version makes one model out of three.
A date is not one of them either, and a date is often four digits:
`deepseek-v4-pro-0813' is a snapshot of `deepseek-v4-pro', so the rule
that drops a trailing number looks for four.  Never fewer: that would eat
the `4' of `gpt-4' and the `8' of `llama-3.1-8b'.

`-v' and `-chat' are also spellings of the same model, so `z-ai/glm-4.6v'
meets benchlm's `Z.ai/GLM-4.6' and `openai/gpt-5.2-chat' meets
`OpenAI/GPT-5.2'.

Add a rule at the position that keeps the order meaningful, and add the ID
pair it fixes to lisp/llm-pick-align-test.el.  Emacs regexps have no digit
class, write [0-9] instead of the backslash-d form used by other tools."
  :type '(alist :key-type regexp :value-type string)
  :group 'llm-pick)

(defun llm-pick-align--normalize-steps (id)
  "Return what ID normalizes to after each rule.
The first element is ID downcased and every following element is the
result of one more rule of `llm-pick-normalize-rules', so the last one is
what `llm-pick-align--normalize' returns."
  (let ((result (downcase (format "%s" (or id ""))))
        (steps nil))
    (push result steps)
    (dolist (rule llm-pick-normalize-rules (nreverse steps))
      (setq result (replace-regexp-in-string (car rule) (cdr rule) result))
      (push result steps))))

(defun llm-pick-align--normalize (id)
  "Return the canonical form of ID.
Normalizing an already normalized ID returns it unchanged."
  (car (last (llm-pick-align--normalize-steps id))))

(defun llm-pick-align--normalize-culprit (a b)
  "Return the rule that first normalizes A and B to the same text, or nil.
Nil means no single rule does it.  That is the rule to drop or tighten
when they name two different models."
  (cl-loop for step-a in (llm-pick-align--normalize-steps a)
           for step-b in (llm-pick-align--normalize-steps b)
           for rule in (cons nil llm-pick-normalize-rules)
           when (and rule (equal step-a step-b))
           return rule))

(defun llm-pick-align--problem (type description)
  "Signal an error of TYPE with DESCRIPTION as its data.
This stops the run at the first problem, which is the intended
behavior now that `llm-pick-align-check' is gone."
  (signal type (list description)))

;;; Alignment

(defvar llm-pick-align--last-report nil
  "The report returned by the last call to `llm-pick-align'.")

(defun llm-pick-align--rank (normalized canonicals)
  "Score NORMALIZED against the anchor candidates in CANONICALS.
Each candidate is an anchor ID paired with its normalized
form.  Return a list of canonical-plus-score pairs sorted by
decreasing score."
  (sort (mapcar (lambda (candidate)
                  (cons (cdr candidate)
                        (llm-pick-align-similarity-score normalized (cdr candidate))))
                canonicals)
        (lambda (a b) (> (cdr a) (cdr b)))))

(defun llm-pick-align--promotions (sources canonicals)
  "Return the IDs several sources agree on and the anchor model each implies.
Return a hash table of NORMALIZED to (CANONICAL . SOURCES).

A normalized ID that at least `llm-pick-align-match-consensus-sources' of
SOURCES arrived at on their own is a model those sources agree about.
When it comes within `llm-pick-align-match-consensus-band' below
`llm-pick-align-match-threshold' of one anchor model, that agreement settles the
match: two catalogues that independently spell a name the same way did not
both invent it, and the score alone would have left the pair apart.

The agreement has to come from a source other than the one being aligned,
and the anchor spells its own IDs, so the rule needs a collection of three
catalogues or more to fire at all.  The band is what keeps agreement from
being the only evidence: a name several sources share but that scores far
below the threshold is still a model of its own.

CANONICALS is the anchor candidate list that `llm-pick-align--rank' scores
against."
  (let ((table (make-hash-table :test #'equal))
        (spellings (make-hash-table :test #'equal))
        ;; One source spelling a name is what a single score already
        ;; says, so consensus needs two at the very least.
        (wanted (max 2 llm-pick-align-match-consensus-sources)))
    (dolist (source-entry sources)
      (let ((source (car source-entry)))
        (dolist (id (cdr source-entry))
          (let* ((normalized (llm-pick-align--normalize id))
                 (seen (gethash normalized spellings)))
            (unless (memq source seen)
              (puthash normalized (cons source seen) spellings))))))
    (maphash
     (lambda (normalized seen)
       (when (>= (length seen) wanted)
         (let ((best (car (llm-pick-align--rank normalized canonicals))))
           (when best
             (let ((score (or (cdr best) 0.0)))
               (when (and (< score llm-pick-align-match-threshold)
                          (>= score (- llm-pick-align-match-threshold
                                       llm-pick-align-match-consensus-band)))
                 (puthash normalized
                          (cons (car best) (nreverse seen))
                          table)))))))
     spellings)
    table))

(defun llm-pick-align--entry (source id result)
  "Return the reviewable entry of one ID that RESULT aligned.
SOURCE names the source the ID came from.
The entry is what `llm-pick-render-report-alignment' prints: the source, the ID
as that source spells it, the form the normalizer turned it into, the
canonical ID it was taken for and how the two were brought together.
Keeping it here is what lets the report show the decision instead of
reconstructing it from the mapping."
  (list :source source
        :id id
        :norm (plist-get result :norm)
        :canonical (plist-get result :canonical)
        :kind (plist-get result :kind)
        :best (plist-get result :best)
        :score (plist-get result :score)))

(defun llm-pick-align--promoted (promotions used)
  "Describe the promotions USED, looking their details up in PROMOTIONS.
USED lists the normalized IDs a run promoted, in the order it met them.
Return one plist per ID, with the anchor model it joined and the sources
that spelled it alike."
  (cl-loop for normalized in (delete-dups used)
           for entry = (gethash normalized promotions)
           when entry
           collect (list :norm normalized
                         :canonical (car entry)
                         :sources (cdr entry))))

(defun llm-pick-align--anchor-index (anchor ids)
  "Index the IDS of the anchor source ANCHOR.
Return a plist (:index HASH :canonicals LIST).  HASH maps a normalized
ID onto the anchor ID it came from, LIST holds (ANCHOR-ID . NORMALIZED)
in the order of IDS.
Report `llm-pick-align-conflict' when two IDs normalize alike, see
`llm-pick-align--align-problem' for who hears about it."
  (let ((index (make-hash-table :test #'equal))
        canonicals)
    (dolist (id ids)
      (let* ((normalized (llm-pick-align--normalize id))
             (previous (gethash normalized index)))
        (when previous
          ;; Plain %s, and no quote character anywhere in the text:
          ;; `error-message-string' re-renders the data, and every quote
          ;; it finds comes back to the user as a backslash-escaped one.
          (llm-pick-align--problem
           'llm-pick-align-conflict
           (format "Source %s lists %s and %s, which both normalize to %s.\n%s"
                   anchor previous id normalized
                   (let ((culprit (llm-pick-align--normalize-culprit previous id)))
                     (if culprit
                         (format "The rule %s -> %s collapses them; drop or tighten it in `llm-pick-align-normalize-rules'."
                                 (car culprit) (cdr culprit))
                       "No single rule does it; check `llm-pick-align-normalize-rules' as a whole.")))))
        ;; The first ID of a collision keeps the canonical ID.  The others
        ;; are left out of the index on purpose: two candidates with the
        ;; same normalized form would tie in `llm-pick-align--rank', and every ID
        ;; that matches them would look ambiguous.  They still reach the
        ;; canonical ID through `llm-pick-align--align', which maps its input.
        (unless previous
          (puthash normalized id index)
          (push (cons id normalized) canonicals))))
    (list :index index :canonicals (nreverse canonicals))))

(defconst llm-pick-align--gap-epsilon 1e-6
  "Slack allowed when a match gap is compared with its threshold.
Two scores that differ only by the rounding of the arithmetic that
produced them are a tie, not a gap.  Without the slack a gap of 0.050,
computed as 0.04999999999999993, counts as smaller than a threshold of
0.05, and the message ends up contradicting the number it just printed.")

(defun llm-pick-align--id (source id index canonicals &optional promoted)
  "Align ID of SOURCE against the anchor described by INDEX and CANONICALS.
INDEX and CANONICALS come from `llm-pick-align--anchor-index'.
Return a plist (:canonical CANONICAL :warning WARNING); WARNING is nil
for a clean match.  Signal `llm-pick-align-ambiguous' when the two best
matches are closer than `llm-pick-align-match-ambiguity-gap', and
`llm-pick-align-unmatched' when nothing reaches
`llm-pick-align-match-threshold' and `llm-pick-align-on-unmatched' is
`error'.

A reported problem still returns the best canonical ID it can find, so
that a collecting run carries on to the next ID instead of giving up on
the whole catalogue.

PROMOTED is the anchor model several sources agree this ID is, as
`llm-pick-align--promotions' found it, or nil.  It settles a match whose
score on its own came out just under `llm-pick-align-match-threshold'."
  (let ((normalized (llm-pick-align--normalize id)))
    (if (gethash normalized index)
        ;; Spelled the anchor's way already, nothing left to decide.
        (list :canonical normalized :norm normalized :kind 'exact)
      (let* ((ranked (llm-pick-align--rank normalized canonicals))
             (best (car ranked))
             (runner-up (cadr ranked))
             (best-score (or (cdr best) 0.0))
             (warning (list :source source :id id :norm normalized
                            :best (car best) :score best-score)))
        (cond
         ((and promoted (< best-score llm-pick-align-match-threshold))
          ;; Several sources spelled this ID the same way and the anchor
          ;; has a model within the consensus band of it, so the
          ;; agreement is what settles the match.
          (list :canonical promoted :norm normalized :kind 'agreed
                :best (car best) :score best-score :promoted normalized))
         ((< best-score llm-pick-align-match-threshold)
          (when (eq llm-pick-align-on-unmatched 'error)
            (signal 'llm-pick-align-unmatched
                    (list source id
                          (format "%s normalizes to %s, whose best match is %s (%.3f), below `llm-pick-align-match-threshold' (%.2f)."
                                  id normalized (or (car best) "none")
                                  best-score llm-pick-align-match-threshold))))
          ;; Keeping the ID as a model of its own is the answer both for
          ;; `llm-pick-align-on-unmatched' and for a collecting run, which needs
          ;; a canonical ID to carry on with.
          (list :canonical normalized :norm normalized :kind 'unmatched
                :best (car best) :score best-score :warning warning))
         ((and runner-up
               (< (- best-score (cdr runner-up))
                  (- llm-pick-align-match-ambiguity-gap llm-pick-align--gap-epsilon)))
          (signal 'llm-pick-align-ambiguous
                  (list source id
                        (format "%s normalizes to %s and matches several anchor IDs:\n  %s (%.3f)\n  %s (%.3f)\nThe gap %.3f is below `llm-pick-align-match-ambiguity-gap' (%.2f).\nFix: adjust `llm-pick-align-similarity-fns', `llm-pick-align-match-threshold' or `llm-pick-align-match-ambiguity-gap'."
                                id normalized
                                (car best) best-score
                                (car runner-up) (cdr runner-up)
                                (- best-score (cdr runner-up))
                                llm-pick-align-match-ambiguity-gap)))
          (list :canonical (car best) :norm normalized :kind 'fuzzy
                :best (car best) :score best-score))
         (t
          (list :canonical (car best) :norm normalized :kind 'fuzzy
                :best (car best) :score best-score)))))))

(defun llm-pick-align--align (sources &optional anchor)
  "Align model IDs across SOURCES.
SOURCES is an alist of (SOURCE . ID-LIST).  ANCHOR names the primary
source and defaults to the value of
`llm-pick-core-default-capability-source'; its IDs define the
canonical IDs, which are their normalized forms.

Return a plist (:mapping :warnings :standalone :promoted :entries).
:mapping is an alist ((SOURCE . ID) . CANONICAL) that covers every
input ID, anchor IDs first.  :warnings describes the IDs that were
kept as standalone models, :standalone lists their canonical IDs, and
:promoted describes the IDs several sources agreed on, see
`llm-pick-align--promotions'.

:entries is the same alignment one ID at a time, in input order and
anchor first: a list of plists with :source, :id, :norm, :canonical,
:kind and :score.  :kind is `anchor' for the anchor's own IDs, `exact'
when the ID normalizes to the anchor's model, `fuzzy' when a
similarity score matched it, `agreed' when several sources spelled it
alike and `unmatched' when nothing matched.  The renderer prints that
list, so that a reader can check every decision `llm-pick' made instead
of only the ones that failed.

Signal `llm-pick-align-conflict' for an anchor source that maps two
different IDs onto one canonical ID, `llm-pick-align-ambiguous' for a
match that is too close to call, `llm-pick-align-unmatched' for an
unmatched ID when `llm-pick-align-on-unmatched' is `error', and
`llm-pick-error' when ANCHOR is missing from SOURCES."
  (let* ((anchor (or anchor (symbol-value 'llm-pick-core-default-capability-source)))
         (anchor-ids (alist-get anchor sources)))
    (unless anchor-ids
      (signal 'llm-pick-error
              (list (format "Anchor source %S has no IDs; sources are %S."
                            anchor (mapcar #'car sources)))))
    (let* ((parsed (llm-pick-align--anchor-index anchor anchor-ids))
           (index (plist-get parsed :index))
           (canonicals (plist-get parsed :canonicals))
           (promotions (llm-pick-align--promotions sources canonicals))
           ;; Every input ID gets a mapping entry, the ones a reported
           ;; collision left out of CANONICALS included: a collecting run
           ;; has to be able to merge the records it collected.
           (anchor-mapping (cl-loop for id in anchor-ids
                                    collect (cons (cons anchor id)
                                                  (llm-pick-align--normalize id))))
           (other-mapping nil)
           (other-entries nil)
           (warnings nil)
           (standalone nil)
           (promoted nil))
      (dolist (source-entry (cl-remove anchor sources :key #'car))
        (let ((source (car source-entry)))
          (dolist (id (cdr source-entry))
            (let* ((normalized (llm-pick-align--normalize id))
                   (entry (gethash normalized promotions))
                   (result (llm-pick-align--id source id index canonicals
                                               (car-safe entry)))
                   (canonical (plist-get result :canonical))
                   (warning (plist-get result :warning)))
              (when (plist-get result :promoted)
                (push normalized promoted))
              (when warning
                (push warning warnings)
                (push canonical standalone))
              (push (llm-pick-align--entry source id result) other-entries)
              (push (cons (cons source id) canonical) other-mapping)))))
      (let ((report (list :mapping (append anchor-mapping (nreverse other-mapping))
                          :warnings (nreverse warnings)
                          :standalone (delete-dups standalone)
                          :promoted (llm-pick-align--promoted
                                     promotions (nreverse promoted))
                          :entries (append
                                    (cl-loop for id in anchor-ids
                                             for normalized = (llm-pick-align--normalize id)
                                             collect (list :source anchor
                                                           :id id
                                                           :norm normalized
                                                           :canonical normalized
                                                           :kind 'anchor))
                                    (nreverse other-entries)))))
        (setq llm-pick-align--last-report report)
        report))))

(provide 'llm-pick-align)

;;; llm-pick-align.el ends here
