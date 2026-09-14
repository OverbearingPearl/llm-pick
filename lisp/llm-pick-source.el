;;; llm-pick-source.el --- Source registry and built-in loaders -*- lexical-binding: t; -*-

;; Copyright (C) 2025 madachuan
;; Author: madachuan <madachuan.noreply.github.com>
;; Assisted-by: Claude
;; URL: https://github.com/madachuan/llm-pick
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;
;; A source is a named provider of model data.  Sources are registered in
;; `llm-pick-source-sources' as an alist (NAME . DESCRIPTOR), where DESCRIPTOR
;; is a plist:
;;
;;   :kind        capability, price or both
;;   :description one line for the user
;;   :loader      function called with an options plist, returning
;;                entries; it reads the offline snapshot
;;   :fetcher     function of the same shape that reads the service
;;                itself, optional; it is used unless `llm-pick-source-offline'
;;                is non-nil
;;   :fixture     file name of the offline snapshot, looked up in
;;                `llm-pick-source-fixture-directory'
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
;; By default a source reads its service over the network and caches the
;; answer in `llm-pick-cache-dir'; with `llm-pick-source-offline' non-nil it
;; reads its snapshot instead.  The snapshots in test/fixtures are
;; illustrative samples, not real quotes, which is why the test suite
;; binds `llm-pick-source-offline' to t and never opens a socket.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url-util)
(require 'llm-pick-core)
(require 'llm-pick-align)
(require 'llm-pick-fetch-get)

;; The user options live in llm-pick.el, which requires this module.
(defvar llm-pick-source-fixture-directory)
(defvar llm-pick-source-offline)
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

(defun llm-pick-source--fixture (name)
  "Return the absolute name of the snapshot that backs source NAME."
  (let ((fixture (plist-get (llm-pick-source--descriptor name) :fixture)))
    (when fixture
      (expand-file-name fixture llm-pick-source-fixture-directory))))

;;; Snapshot loading

(defun llm-pick-source--read-json (file)
  "Return the JSON content of FILE as hash tables, lists and scalars.
Objects become hash tables with string keys.  Signal `llm-pick-error'
when FILE does not exist."
  (unless (file-readable-p file)
    (signal 'llm-pick-error
            (list (format "Source snapshot not found: %s" file))))
  ;; Same reasoning as the service cache: JSON is UTF-8 by definition, so
  ;; the encoding of a snapshot is not something to ask about, and naming
  ;; it here keeps a snapshot with an unclear encoding from prompting.
  (let ((coding-system-for-read 'utf-8))
    (llm-pick-core--parse-json (with-temp-buffer
                            (insert-file-contents file)
                            (buffer-string)))))

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

(defun llm-pick-source--fixture-score (model category)
  "Return the capability score of a parsed JSON MODEL.
CATEGORY selects one entry of the \"scores\" object; nil means the
highest score of any category."
  (let ((scores (llm-pick-source--json-field model "scores")))
    (when (hash-table-p scores)
      (if category
          (gethash category scores)
        (let (best)
          (maphash (lambda (_category score)
                     (when (and (numberp score) (or (null best) (> score best)))
                       (setq best score)))
                   scores)
          best)))))

(defun llm-pick-source--fixture-prices (model)
  "Return the :in and :out price plist of a parsed JSON MODEL, or nil."
  (let ((pricing (llm-pick-source--json-field model "pricing")))
    (when (hash-table-p pricing)
      (let ((in (gethash "prompt" pricing))
            (out (gethash "completion" pricing)))
        (when (or (numberp in) (numberp out))
          (list :in in :out out))))))

(defun llm-pick-source--fixture-entry (model kind category)
  "Convert one parsed JSON MODEL into a source entry for a source of KIND.
CATEGORY selects the capability score to use, see
`llm-pick-source--fixture-score'."
  (let ((id (llm-pick-source--json-field model "id")))
    (when id
      (append (list :id id
                    :display-name (or (llm-pick-source--json-field model "name") id)
                    :providers (llm-pick-source--provider-ids model))
              (when (memq kind '(capability both))
                (let ((score (llm-pick-source--fixture-score model category)))
                  (when (numberp score)
                    (list :score score :category category))))
              (when (memq kind '(price both))
                (let ((prices (llm-pick-source--fixture-prices model)))
                  (when prices (list :prices prices))))))))

(defun llm-pick-source--fixture-loader (options)
  "Return the entries stored in the snapshot named in OPTIONS.
OPTIONS is a plist with :kind, :category and :fixture.  See the
Commentary of `llm-pick-source' for the snapshot layout."
  (let* ((file (plist-get options :fixture))
         (data (llm-pick-source--read-json file))
         (models (gethash "models" data)))
    (unless (listp models)
      (signal 'llm-pick-error
              (list (format "Snapshot %s has no \"models\" array" file))))
    (cl-loop for model in models
             for entry = (llm-pick-source--fixture-entry
                          model (plist-get options :kind)
                          (plist-get options :category))
             when entry collect entry)))

;;; Service loaders

(defun llm-pick-source--benchlm-url (category)
  "Return the leaderboard URL that carries CATEGORY.
A nil CATEGORY asks for every category, and the limit is the maximum the
service documents, so that a report sees every model it tracks."
  (concat llm-pick-fetch-get--benchlm-url
          "?limit=200"
          (when category
            (concat "&category=" (url-hexify-string category)))))

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
selects the score, see `llm-pick-fetch-get-http'.  A model without a score in
that category is left out."
  (let* ((category (plist-get options :category))
         (data (llm-pick-fetch-get-json (llm-pick-source--benchlm-url category)))
         (models (llm-pick-source--json-field data "models")))
    (unless (listp models)
      (signal 'llm-pick-error
              (list "The BenchLM leaderboard has no \"models\" array")))
    (cl-loop for model in models
             for id = (llm-pick-source--benchlm-id model)
             for score = (llm-pick-source--benchlm-score model category)
             when (and id (numberp score))
             collect (list :id id
                           :display-name (or (llm-pick-source--json-field model "model")
                                             id)
                           :score score
                           :category category))))

(defun llm-pick-source--openrouter-headers ()
  "Return the request headers for the OpenRouter model list.
The service takes a bearer token; when none is configured the header is
left out and the service decides, see `llm-pick-source-openrouter-api-key'."
  (let ((key (or llm-pick-source-openrouter-api-key
                 (getenv "OPENROUTER_API_KEY"))))
    (append (when key
              (list (cons "Authorization" (concat "Bearer " key))))
            '(("Accept" . "application/json")))))

(defun llm-pick-source--openrouter-price (pricing key)
  "Return the per million token price of the parsed PRICING at KEY.
OpenRouter quotes a price per token, as a string or as a number, so the
value is scaled by a million.  Scaling by 1e12 and rounding to the
nearest 1e-6 dollar keeps the float noise of the conversion out of the
reports.  Nil when KEY is absent."
  (let ((value (llm-pick-source--json-field pricing key)))
    (when (or (numberp value) (stringp value))
      (let ((per-token (if (numberp value) value (string-to-number value))))
        (/ (round (* per-token 1e12)) 1e6)))))

(defun llm-pick-source--openrouter-prices (model)
  "Return the per million token prices of a parsed OpenRouter MODEL, or nil."
  (let* ((pricing (llm-pick-source--json-field model "pricing"))
         (in (llm-pick-source--openrouter-price pricing "prompt"))
         (out (llm-pick-source--openrouter-price pricing "completion")))
    (when (or (numberp in) (numberp out))
      (list :in in :out out))))

(defun llm-pick-source--openrouter-loader (_options)
  "Return the entries of the OpenRouter model list.
OPTIONS is ignored: the endpoint answers with every model at once, so a
category does not change the answer."
  (let* ((data (llm-pick-fetch-get-json llm-pick-fetch-get--openrouter-url
                                    (llm-pick-source--openrouter-headers)))
         ;; The list is documented as \"data\"; \"models\" is accepted as
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
             collect (append (list :id id
                                   :display-name (or (llm-pick-source--json-field model "name")
                                                     id)
                                   :providers (list (cons 'openrouter id)))
                             (let ((prices (llm-pick-source--openrouter-prices model)))
                               (when prices (list :prices prices)))))))

;;; Built-in sources

(llm-pick-source-register 'benchlm
                          :kind 'capability
                          :description "Capability scores per category"
                          :loader #'llm-pick-source--fixture-loader
                          :fetcher #'llm-pick-source--benchlm-loader
                          :fixture "benchlm-sample.json")

(llm-pick-source-register 'openrouter
                          :kind 'price
                          :description "List prices in USD per million tokens"
                          :loader #'llm-pick-source--fixture-loader
                          :fetcher #'llm-pick-source--openrouter-loader
                          :fixture "openrouter-sample.json")

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
  "Return the loader function for the data of source NAME.
A nil `llm-pick-source-offline' prefers the :fetcher of DESCRIPTOR, which reads
the service; otherwise, and for a source without one, the :loader reads
the offline snapshot.  Signal `llm-pick-error' when the preferred one is
missing, instead of reporting a source with no models."
  (let ((loader (if llm-pick-source-offline
                    (plist-get descriptor :loader)
                  (or (plist-get descriptor :fetcher)
                      (plist-get descriptor :loader)))))
    (unless loader
      (signal 'llm-pick-error
              (list (format "Source %S has no %s; see `llm-pick-source-register'"
                            name (if llm-pick-source-offline ":loader" ":fetcher")))))
    loader))

(defun llm-pick-source--collect-source (name options)
  "Return the entries of source NAME, loaded with OPTIONS."
  (let ((descriptor (llm-pick-source--descriptor name)))
    (unless descriptor
      (signal 'llm-pick-error (list (format "Unknown source: %S" name))))
    (funcall (llm-pick-source--loader name descriptor)
             (append (list :source name
                           :kind (plist-get descriptor :kind)
                           :fixture (llm-pick-source--fixture name))
                     options))))

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

(defun llm-pick-source--merge-entry (record source entry)
  "Return RECORD after merging one ENTRY of SOURCE into it.
A source contributes at most one score, one price plist and one ID per
provider; the first entry that reaches a canonical ID wins.  A record
that spans several categories keys its scores by (SOURCE . CATEGORY),
one that spans a single one keys them by SOURCE."
  (let* ((score (plist-get entry :score))
         (category (plist-get entry :category))
         (score-key (if (and category (cdr (plist-get record :categories)))
                        (cons source category)
                      source))
         (prices (plist-get entry :prices))
         (providers (plist-get entry :providers))
         (name (plist-get entry :display-name))
         (scores (plist-get record :scores))
         (record-prices (plist-get record :prices))
         (record-providers (plist-get record :providers)))
    (when (and score (null (assoc score-key scores)))
      (setq scores (append scores (list (cons score-key score)))))
    (when (and prices (null (alist-get source record-prices)))
      (setq record-prices (append record-prices (list (cons source prices)))))
    (dolist (provider providers)
      (unless (assq (car provider) record-providers)
        (setq record-providers (append record-providers (list provider)))))
    ;; A record always carries these keys, so `plist-put' updates in place.
    (plist-put record :scores scores)
    (plist-put record :prices record-prices)
    (plist-put record :providers record-providers)
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
loaded for.  Return the records ordered by canonical ID."
  (let ((by-canonical (make-hash-table :test #'equal))
        canonicals)
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
              (llm-pick-source--merge-entry record source entry))))))
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
