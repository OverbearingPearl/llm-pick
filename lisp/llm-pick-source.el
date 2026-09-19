;;; llm-pick-source.el --- Source registry and built-in loaders -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: Claude
;; URL: https://github.com/OverbearingPearl/llm-pick
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;
;; A source is a named provider of model data.  Sources are registered in
;; `llm-pick-source-sources' as an alist (NAME . DESCRIPTOR), where DESCRIPTOR
;; is a plist:
;;
;;   :kind        capability, price or both
;;   :description one line for the user
;;   :fetcher     function of the same shape that reads the service
;;                itself, optional; it is used unless `llm-pick-source-offline'
;;                is non-nil
;;
;; A loader returns entries, each a plist:
;;
;;   :id           ID as spelled by the source
;;   :display-name human readable name
;;   :providers    alist (PROVIDER . ID)
;;   :score        capability score 0-100 (capability sources)
;;   :category     the category :score belongs to
;;   :prices       plist (:in USD :out USD) per million tokens (price
;;                 sources)
;;
;; Adding a source never touches another module: register a loader with
;; `llm-pick-source-register' and the alignment, analysis and rendering
;; layers pick it up.
;;

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url-util)
(require 'llm-pick-core)
(require 'llm-pick-align)
(require 'llm-pick-fetch-get)

;; The user options live in llm-pick.el, which requires this module.
(defvar llm-pick-source-openrouter-api-key)

;;; Registry

(defvar llm-pick-source-sources nil
  "Alist of (NAME . DESCRIPTOR) known sources, in registration order.")

(defun llm-pick-source-register (name &rest descriptor)
  "Register the source NAME described by DESCRIPTOR.
DESCRIPTOR is a plist, see the Commentary of `llm-pick-source'.
Re-registering a name replaces its descriptor and keeps its position.
Return NAME."
  (let ((existing (assq name llm-pick-source-sources)))
    (if existing
        (setcdr existing descriptor)
      (setq llm-pick-source-sources
            (append llm-pick-source-sources (list (cons name descriptor))))))
  name)

(defun llm-pick-source--descriptor (name)
  "Return the descriptor of the source NAME, or nil."
  (cdr (assq name llm-pick-source-sources)))

(defun llm-pick-source--names ()
  "Return the names of the registered sources, in registration order."
  (mapcar #'car llm-pick-source-sources))

(defun llm-pick-source--json-field (object key)
  "Return KEY of the parsed JSON OBJECT, or nil.
OBJECT is a hash table as produced by `llm-pick-source--read-json'."
  (when (hash-table-p object) (gethash key object)))

(defun llm-pick-source--provider-ids (model)
  "Return the provider ID alist of a parsed JSON MODEL.
The alist is sorted by provider name so that the result does not depend
on hash table order."
  (let ((ids (llm-pick-source--json-field model "provider_ids")))
    (when (hash-table-p ids)
      (let (result)
        (maphash (lambda (provider id) (push (cons (intern provider) id) result))
                 ids)
        (sort result (lambda (a b) (string< (symbol-name (car a))
                                            (symbol-name (car b)))))))))

;;; Service loaders

(defun llm-pick-source--benchlm-url (category)
  "Return the leaderboard URL for CATEGORY.
A nil CATEGORY asks for every category; otherwise CATEGORY selects one
leaderboard category via the category query parameter."
  (if category
      (concat llm-pick-fetch-get--benchlm-url
              "?category=" (url-hexify-string category))
    llm-pick-fetch-get--benchlm-url))

(defvar llm-pick-source--benchlm-cache nil
  "Short-lived in-process cache of the parsed plain leaderboard.

The leaderboard endpoint answers with every category at once, so the
per-category collector calls share one network fetch through this cache.
It holds a cons cell (URL . (TIME . DATA)), where TIME is the
floating-point time at which DATA was fetched; entries older than a few
seconds are considered stale and refetched.")

(defun llm-pick-source--benchlm-data ()
  "Return the parsed plain leaderboard, caching it for 60 seconds."
  (let ((url llm-pick-fetch-get--benchlm-url))
    (if (and llm-pick-source--benchlm-cache
             (equal (car llm-pick-source--benchlm-cache) url)
             (< (float-time (time-subtract (current-time)
                                           (cadr llm-pick-source--benchlm-cache)))
                60))
        (caddr llm-pick-source--benchlm-cache)
      (let ((data (llm-pick-fetch-get-json url)))
        (setq llm-pick-source--benchlm-cache (list url (current-time) data))
        data))))

(defun llm-pick-source--benchlm-id (model)
  "Return the ID of a parsed BenchLM MODEL.
The leaderboard names a model by its display name and its creator rather
than by an ID, so the ID is \"CREATOR/NAME\"; `llm-pick-align--normalize' turns
that into the same shape the other sources use."
  (let ((name (llm-pick-source--json-field model "model"))
        (creator (llm-pick-source--json-field model "creator")))
    (when name
      (if creator (format "%s/%s" creator name) name))))

(defun llm-pick-source--benchlm-score (model category)
  "Return the score of a parsed BenchLM MODEL in CATEGORY.
CATEGORY nil means the best score of the model over its categories, which
is the rule the snapshot loader follows as well."
  (let ((scores (llm-pick-source--json-field model "categoryScores")))
    (when (hash-table-p scores)
      (if category
          (gethash category scores)
        (let (best)
          (maphash (lambda (_category score)
                     (when (and (numberp score)
                                (or (null best) (> score best)))
                       (setq best score)))
                   scores)
          best)))))

(defun llm-pick-source--benchlm-loader (options)
  "Return the entries of the BenchLM leaderboard.
OPTIONS is the plist `llm-pick-source--collect-source' passes; its :category
selects the score, see `llm-pick-fetch-get-http'.  The endpoint answers with
every category at once, so the fetch is shared across the collector's
per-category calls.  The top-level numeric fields \"inputPrice\",
\"outputPrice\", and \"cachedInputPrice\" hold per-million-token prices; when
at least one of them is a non-negative number, the entry gets a :prices
\& IN :out OUT :cache CACHE) attached, where each half is the field
value when it is a non-negative number and nil otherwise.  When :category is
non-nil, every model with a numeric score in that category is returned with
:score/:category attached.  When :category is nil, every model with an ID is
returned, with :score set to the best numeric score the model has across its
categories and :category nil; \"overallScore\" is used only when the model has
no numeric category score, and a model with no numeric score at all keeps no
:score."
  (let* ((category (plist-get options :category))
         (data (llm-pick-source--benchlm-data))
         (models (llm-pick-source--json-field data "models"))
         (entries nil))
    (unless (listp models)
      (signal 'llm-pick-error
              (list "The BenchLM leaderboard has no \"models\" array")))
    (dolist (model models)
      (let ((id (llm-pick-source--benchlm-id model)))
        (when id
          (let ((display (or (llm-pick-source--json-field model "model")
                             id))
                (in-price (llm-pick-source--json-field model "inputPrice"))
                (out-price (llm-pick-source--json-field model "outputPrice"))
                (cache-price (llm-pick-source--json-field model "cachedInputPrice"))
                prices)
            (when (or (and (numberp in-price) (>= in-price 0))
                      (and (numberp out-price) (>= out-price 0))
                      (and (numberp cache-price) (>= cache-price 0)))
              (setq prices (list :prices (list :in (and (numberp in-price)
                                                        (>= in-price 0)
                                                        in-price)
                                               :out (and (numberp out-price)
                                                         (>= out-price 0)
                                                         out-price)
                                               :cache (and (numberp cache-price)
                                                           (>= cache-price 0)
                                                           cache-price)))))
            (if category
                (let ((score (llm-pick-source--benchlm-score model category)))
                  (when (numberp score)
                    (push (append (list :id id
                                        :display-name display
                                        :score score
                                        :category category)
                                  prices)
                          entries)))
              (let ((best (llm-pick-source--benchlm-score model nil)))
                (cond
                 ((numberp best)
                  (push (append (list :id id
                                      :display-name display
                                      :score best
                                      :category nil)
                                prices)
                        entries))
                 ((numberp (llm-pick-source--json-field model "overallScore"))
                  (push (append (list :id id
                                      :display-name display
                                      :score (llm-pick-source--json-field model "overallScore")
                                      :category nil)
                                prices)
                        entries))
                 (t
                  (push (append (list :id id :display-name display) prices)
                        entries)))))))))
    (nreverse entries)))

(defun llm-pick-source--openrouter-headers ()
  "Return the request headers for the OpenRouter model list.
The service takes a bearer token; when none is configured the header is
left out and the service decides, see `llm-pick-source-openrouter-api-key'."
  (let ((key (or llm-pick-source-openrouter-api-key
                 (getenv "OPENROUTER_API_KEY")
                 (condition-case nil
                     (let* ((entries (or (auth-source-search :host "openrouter.ai"
                                                             :user "api-key"
                                                             :require '(:secret))
                                         (auth-source-search :host "openrouter.ai"
                                                             :require '(:secret))))
                            (secret (plist-get (car entries) :secret)))
                       (when secret
                         (if (functionp secret)
                             (funcall secret)
                           secret)))
                   (error nil)))))
    (append (when key
              (list (cons "Authorization" (concat "Bearer " key))))
            '(("Accept" . "application/json")))))

(defun llm-pick-source--openrouter-price (pricing key)
  "Return the per million token price of the parsed PRICING at KEY.
OpenRouter quotes a price per token, as a string or as a number, so the
value is scaled by a million.  Scaling by 1e12 and rounding to the
nearest 1e-6 dollar keeps the float noise of the conversion out of the
reports.  A negative price, a sentinel such as -1e6 for the routing
pseudo-models, is reported as missing.  Nil when KEY is absent."
  (let ((value (llm-pick-source--json-field pricing key)))
    (when (or (numberp value) (stringp value))
      (let* ((per-token (if (numberp value) value (string-to-number value)))
             (scaled (/ (round (* per-token 1e12)) 1e6)))
        (and (>= scaled 0) scaled)))))

(defun llm-pick-source--openrouter-price-override (pricing)
  "Return the override entry in PRICING covering the current UTC time, or nil.
PRICING is a parsed JSON hash table; the \"overrides\" field is looked
up via `llm-pick-source--json-field', as are each entry's
\"utc_start\" and \"utc_end\" fields."
  (let* ((now (decode-time (current-time) t))
         (now-hm (+ (* (nth 2 now) 100) (nth 1 now)))
         (overrides (llm-pick-source--json-field pricing "overrides")))
    (when (listp overrides)
      (seq-find
       (lambda (ov)
         (let ((start (llm-pick-source--json-field ov "utc_start"))
               (end (llm-pick-source--json-field ov "utc_end")))
           (when (and (numberp start) (numberp end))
             (if (> start end)
                 (or (>= now-hm start) (< now-hm end))
               (and (>= now-hm start) (< now-hm (if (zerop end) 2400 end)))))))
       overrides))))

(defun llm-pick-source--openrouter-prices (model)
  "Return the per million token prices of a parsed OpenRouter MODEL, or nil.
Prices are read from the active time-of-day override (see
`llm-pick-source--openrouter-price-override'), falling back to the base
pricing object when no override applies.  Includes the cache-hit
input price; :cache is nil when the provider does not quote one."
  (let* ((pricing (llm-pick-source--json-field model "pricing"))
         (active (or (llm-pick-source--openrouter-price-override pricing)
                     pricing))
         (in (llm-pick-source--openrouter-price active "prompt"))
         (out (llm-pick-source--openrouter-price active "completion"))
         (cache (llm-pick-source--openrouter-price active "input_cache_read")))
    (when (or (numberp in) (numberp out))
      (list :in in :out out :cache cache))))

(defun llm-pick-source--openrouter-loader (_options)
  "Return the entries of the OpenRouter model list.
OPTIONS is ignored: the endpoint answers with every model at once, so a
category does not change the answer.
For a ~-prefixed alias the :providers slot still carries the raw alias
ID; when the merge folds the alias into its canonical slug entry the
raw ID is demoted to :provider-aliases, so the main view can show both
as \"slug <- ~alias\"."
  (cl-flet ((normalized-name (name)
              "Strip a leading \"Vendor: \" prefix from NAME, BenchLM style.
Only the first colon followed by a space counts, so names like
\"GLM 5: Turbo\" keep their inner colon."
              (replace-regexp-in-string "\\`[^:]*: " "" name)))
    (let* ((data (llm-pick-fetch-get-json llm-pick-fetch-get--openrouter-url
                                          (llm-pick-source--openrouter-headers)))
           ;; The list is documented as "data"; "models" is accepted as
           ;; well, so that a differently wrapped answer is read rather
           ;; than reported as an empty catalogue.
           (models (or (llm-pick-source--json-field data "data")
                       (llm-pick-source--json-field data "models"))))
      (unless (listp models)
        (signal 'llm-pick-error
                (list "The OpenRouter model list has no model array")))
      (cl-loop for model in models
               for id = (llm-pick-source--json-field model "id")
               when id
               ;; A ~-prefixed latest alias points at a concrete version via
               ;; "alias_target"; use the target slug as the canonical id so
               ;; the alias merges into that version instead of staying an
               ;; unmatched ~-prefixed entry.
               for alias-target = (llm-pick-source--json-field model "alias_target")
               for slug = (and alias-target
                               (llm-pick-source--json-field alias-target "slug"))
               for canonical-id = (or slug id)
               collect (append (list :id canonical-id
                                     :display-name
                                     (normalized-name
                                      (or (llm-pick-source--json-field model "name")
                                          canonical-id))
                                     :providers (list (cons 'openrouter id)))
                               (when slug (list :alias t))
                               (let ((prices (llm-pick-source--openrouter-prices model)))
                                 (when prices (list :prices prices))))))))

(defun llm-pick-source--openrouter-benchmarks-loader (options)
  "Return entries from the OpenRouter benchmarks endpoint.

These are the Artificial Analysis capability indices.  OPTIONS'
:category selects which index becomes the entry's :score:
\"intelligence\" -> intelligence_index, \"coding\" -> coding_index,
\"agentic\" -> agentic_index.  When nil, emit one entry per index
with the matching category."
  (let* ((data (llm-pick-source--json-field
                (llm-pick-fetch-get-json
                 llm-pick-fetch-get--openrouter-benchmarks-url
                 (llm-pick-source--openrouter-headers))
                "data"))
         (selected (plist-get options :category))
         (indices '(("intelligence" . "intelligence_index")
                    ("coding" . "coding_index")
                    ("agentic" . "agentic_index")))
         (wanted (if selected (list (assoc selected indices)) indices))
         entries)
    (unless (listp data)
      (signal 'llm-pick-error '("OpenRouter benchmarks response has no \"data\" array")))
    (dolist (item data)
      (let ((slug (llm-pick-source--json-field item "model_permaslug")))
        (when slug
          (dolist (spec wanted)
            (let ((value (llm-pick-source--json-field item (cdr spec))))
              (when (numberp value)
                (push (list :id slug
                            :display-name (llm-pick-source--json-field item "display_name")
                            :providers nil
                            :score value
                            :category (car spec))
                      entries)))))))
    (nreverse entries)))

;;; Built-in sources

(llm-pick-source-register 'benchlm
                          :kind 'capability
                          :description "Capability scores per category"
                          :fetcher #'llm-pick-source--benchlm-loader)

(llm-pick-source-register 'openrouter
                          :kind 'both
                          :description "List prices in USD per million tokens, plus the Artificial Analysis capability indices"
                          :fetcher (list #'llm-pick-source--openrouter-loader
                                         #'llm-pick-source--openrouter-benchmarks-loader))

;;; Collection

(defun llm-pick-source--categories (category)
  "Return CATEGORY as a list of category names.
A string, a symbol or nil selects one category, a list selects several.
Signal `llm-pick-error' for anything else: `:category' with a typo in it
must not quietly return a report whose scores are all missing.

Nil is `(list nil)', not the empty list: `listp' accepts nil, so reading
it as \"no category at all\" would collect no entries and report every
model without a score."
  (let ((categories (cond ((null category) (list nil))
                          ((listp category) category)
                          (t (list category)))))
    (dolist (item categories)
      (unless (or (null item) (stringp item) (symbolp item))
        (signal 'llm-pick-error
                (list (format "Category must be a string, a symbol or a list of them: %S"
                              category)))))
    (mapcar #'llm-pick-core--category-name categories)))

(defun llm-pick-source--loader (name descriptor)
  "Return the fetcher function for source NAME, read from its DESCRIPTOR.
A list-valued :fetcher means several fetchers whose entry lists are
appended in order.  Signal `llm-pick-error' when the source lacks
a :fetcher."
  (let ((fetcher (plist-get descriptor :fetcher)))
    (cond
     ((functionp fetcher) fetcher)
     ((and (consp fetcher) (not (functionp fetcher)))
      (lambda (options)
        (apply #'append
               (mapcar (lambda (f) (funcall f options)) fetcher))))
     (t (signal 'llm-pick-error
                (list (format "Source %S has no :fetcher; see `llm-pick-source-register'"
                              name)))))))

(defun llm-pick-source--collect-source (name options)
  "Return the entries of source NAME, loaded with OPTIONS."
  (let ((descriptor (llm-pick-source--descriptor name)))
    (unless descriptor
      (signal 'llm-pick-error (list (format "Unknown source: %S" name))))
    (let ((loader (llm-pick-source--loader name descriptor)))
      (funcall loader
               (append (list :source name
                             :kind (plist-get descriptor :kind))
                       options)))))

(defun llm-pick-source--collect-anchor (names anchor)
  "Return the source whose IDs define the canonical IDs.
ANCHOR is used when it is one of NAMES, otherwise
`llm-pick-core-default-capability-source' when it is, otherwise the first of
NAMES."
  (let ((default-source
         (symbol-value 'llm-pick-core-default-capability-source)))
    (cond
     ((memq anchor names) anchor)
     ((memq default-source names)
      default-source)
     (t (car names)))))

(defun llm-pick-source--merge-entry (record source entry &optional quality)
  "Return RECORD after merging one ENTRY of SOURCE into it.
A source contributes at most one score, one price plist and one ID per
provider; the best-attested entry of a source wins each slot (see
QUALITY).  An entry with a :category is keyed by the pair
\\(SOURCE . CATEGORY\\) so its column can be read per category; only an
entry without a category is keyed by SOURCE.

QUALITY is a number (higher means a better-attested ID, as computed by
the caller from the alignment entry's kind and score).  For providers,
the best-attested ID wins the provider slot rather than the first: the
quality of the ID each provider name currently holds is tracked in the
record's :provider-rank alist as (PROVIDER-NAME . QUALITY).  Only an
entry whose :alias is non-nil contributes to the record's
:provider-aliases list: aliases are the source's own alias IDs (such as
a ~-prefixed latest alias), never the concrete IDs merged into the
record itself.  Every alias that loses the provider slot, whether it
came before the winner or after it, is kept so that the main view can
show the alias pointing at the concrete version it was merged into.
Aliases are deduplicated: repeated merges (one per category, into the
same record) never stack the same alias, and an alias never repeats the
current winner's ID."
  (let* ((score (plist-get entry :score))
         (category (plist-get entry :category))
         (score-key (if category
                        (cons source (llm-pick-core--category-name category))
                      source))
         (prices (plist-get entry :prices))
         (providers (plist-get entry :providers))
         (name (plist-get entry :display-name))
         (aliasp (plist-get entry :alias))
         (scores (plist-get record :scores))
         (record-prices (plist-get record :prices))
         (record-providers (plist-get record :providers))
         (provider-rank (plist-get record :provider-rank))
         (provider-aliases (plist-get record :provider-aliases))
         alias-candidate
         winner-id)
    (when (and score (null (assoc score-key scores)))
      (setq scores (append scores (list (cons score-key score)))))
    (when (and prices (null (alist-get source record-prices)))
      (setq record-prices (append record-prices (list (cons source prices)))))
    (dolist (provider providers)
      (let* ((provider-name (car provider))
             (stored (assq provider-name record-providers))
             (stored-quality (cdr (assq provider-name provider-rank))))
        (cond
         ((null stored)
          (setq record-providers (append record-providers (list provider))
                provider-rank (append provider-rank
                                      (list (cons provider-name
                                                  (or quality 0))))
                winner-id (cdr provider)))
         ((and quality (numberp stored-quality)
               (> quality stored-quality))
          ;; A better-attested ID takes the provider slot; an alias it
          ;; displaces stays on as an alias.
          (setq alias-candidate (and aliasp (cdr stored))
                record-providers (append (delete stored record-providers)
                                         (list provider))
                provider-rank (cons (cons provider-name quality)
                                    (assq-delete-all provider-name
                                                     provider-rank))
                winner-id (cdr provider)))
         (t
          ;; A lesser-attested ID keeps its alias next to the winner.
          (setq alias-candidate (and aliasp (cdr provider))
                winner-id (cdr stored))))
        (when alias-candidate
          ;; Drop the candidate if it is the ID currently in (or being
          ;; installed into) the provider slot, so the winner itself
          ;; never lands in the alias list.
          (unless (equal alias-candidate winner-id)
            ;; Drop the candidate if already recorded as an alias, so
            ;; repeated merges do not stack.
            (setq provider-aliases
                  (append (delete alias-candidate provider-aliases)
                          (list alias-candidate))))
          (setq alias-candidate nil))))
    (setq provider-aliases (delete-dups provider-aliases))
    ;; A record always carries these keys, so `plist-put' updates in place.
    (plist-put record :scores scores)
    (plist-put record :prices record-prices)
    (plist-put record :providers record-providers)
    (plist-put record :provider-rank provider-rank)
    (plist-put record :provider-aliases provider-aliases)
    ;; Keep the canonical ID as display name until a source names it.
    (when (and name (equal (plist-get record :display-name)
                           (plist-get record :canonical)))
      (plist-put record :display-name name))
    record))

(defun llm-pick-source--finalize (record)
  "Return RECORD with its :scope derived from the data it carries."
  (plist-put record :scope
             (let ((capability (not (null (plist-get record :scores))))
                   (price (not (null (plist-get record :prices)))))
               (cond ((and capability price) 'both)
                     (capability 'capability-only)
                     (price 'price-only)
                     (t 'unknown)))))

(defun llm-pick-source--combine (entries report categories)
  "Merge the per-source ENTRIES into model records using REPORT.
ENTRIES is an alist (SOURCE . ENTRY-LIST), REPORT comes from
`llm-pick-align--align' and CATEGORIES lists the categories the entries were
loaded for.  Alias entries are ranked half an exact one so that the concrete
slug always outranks its ~-prefixed alias.  Return the records ordered by
canonical ID."
  (let ((by-canonical (make-hash-table :test #'equal))
        (quality (make-hash-table :test #'equal))
        canonicals)
    (dolist (aligned (plist-get report :entries))
      (let* ((kind (plist-get aligned :kind))
             (kind-rank (cond
                         ((eq kind 'exact) 3)
                         ((eq kind 'agreed) 2)
                         ((eq kind 'fuzzy) 1)
                         (t 0)))
             (kind-rank (if (plist-get aligned :alias)
                            (* 0.5 kind-rank)
                          kind-rank))
             (pair (cons (plist-get aligned :source)
                         (plist-get aligned :id))))
        (puthash pair
                 (+ (* 10 kind-rank) (or (plist-get aligned :score) 0))
                 quality)))
    (dolist (source-entry entries)
      (let ((source (car source-entry)))
        (dolist (entry (cdr source-entry))
          (let* ((id (plist-get entry :id))
                 (canonical (cdr (assoc (cons source id)
                                        (plist-get report :mapping)))))
            (unless canonical
              (signal 'llm-pick-error
                      (list (format "Source %S entry %S is missing from the alignment report"
                                    source id))))
            (let ((record (gethash canonical by-canonical)))
              (unless record
                (setq record (llm-pick-core--make-record canonical
                                                    :categories categories))
                (puthash canonical record by-canonical)
                (push canonical canonicals))
              (llm-pick-source--merge-entry record source entry
                                            (gethash (cons source id)
                                                     quality 0)))))))
    (mapcar (lambda (canonical)
              (llm-pick-source--finalize (gethash canonical by-canonical)))
            (sort canonicals #'string<))))

(defun llm-pick-source--collect (&rest args)
  "Collect model records from the registered sources.
ARGS is a plist accepted by `llm-pick-collect': :category selects the
capability category, or a list of categories to collect side by side
\(nil means the best score of any category\), :sources lists the source
names (nil means every registered source) and :anchor names the source
whose IDs define the canonical IDs.

Return the records ordered by canonical ID.  Each record carries the
score of every capability source, the prices of every price source and
the provider IDs of every source that named it; a record collected for
several categories keys its scores by a pair (SOURCE . CATEGORY)."
  (let* ((names (or (plist-get args :sources) (llm-pick-source--names)))
         (categories (llm-pick-source--categories (plist-get args :category))))
    (unless names
      (signal 'llm-pick-error (list "No sources to collect from")))
    (let* ((entries (cl-loop for name in names
                             collect (cons name
                                           (cl-loop for category in categories
                                                    append (llm-pick-source--collect-source
                                                            name
                                                            (list :category category))))))
           (anchor (llm-pick-source--collect-anchor names (plist-get args :anchor)))
           (report (llm-pick-align--align
                    (cl-loop for (name . items) in entries
                             collect (cons name
                                           (delete-dups
                                            (mapcar (lambda (item)
                                                      (plist-get item :id))
                                                    items))))
                    anchor)))
      (llm-pick-source--combine entries report categories))))

(provide 'llm-pick-source)

;;; llm-pick-source.el ends here
