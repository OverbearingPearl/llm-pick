;;; llm-pick-test.el --- Test entry point -*- lexical-binding: t; -*-

;;; Commentary:

;;
;; Single entry point for the test suite.  `llm-pick-test-run' reloads
;; every module and then every test file from disk, so a running Emacs
;; always tests the sources on disk instead of stale definitions.
;;
;;   M-x llm-pick-test-run
;;   Emacs -Q --batch -L . -l llm-pick-test.el -f llm-pick-test-run
;;
;; User options the user set survive the reload: they are snapshotted
;; before the modules are unloaded and restored afterwards, so a test run
;; never throws away the configuration of a running session.  Options
;; that merely hold the default of an already loaded revision are dropped
;; instead; otherwise a running Emacs would keep testing the old defaults
;; no matter how often the sources change on disk.

;;; Code:

(require 'cl-lib)
(require 'ert)

(defconst llm-pick-test--root
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory holding llm-pick.el and this file.")

(defconst llm-pick-test--lisp-dir
  (expand-file-name "lisp" llm-pick-test--root)
  "Directory holding the implementation modules and their tests.")

(defconst llm-pick-test--module-regexp "\\`llm-pick-.*\\.el\\'"
  "Regexp matching an implementation module file name.")

(defconst llm-pick-test--test-regexp "\\`llm-pick-.*-test\\.el\\'"
  "Regexp matching a test file name.")

(defconst llm-pick-test--reset-vars
  '(llm-pick-align--last-report
    llm-pick-source-sources
    llm-pick-source--benchlm-cache)
  "Variables that must not survive a reload.
Only caches belong here.  Defcustoms are covered by
`llm-pick-test--stale-option-p' and `llm-pick-test--user-options'; do
not add them to this list.")

(dolist (dir (list llm-pick-test--lisp-dir llm-pick-test--root))
  (add-to-list 'load-path dir))

(defun llm-pick-test--files (regexp)
  "Return the absolute names of the files in lisp/ matching REGEXP."
  (cl-remove-if-not
   (lambda (file) (string-match-p regexp (file-name-nondirectory file)))
   (directory-files llm-pick-test--lisp-dir t)))

(defun llm-pick-test--module-files ()
  "Return the implementation module files, in directory order.
The test files match `llm-pick-test--module-regexp' as well, so they are
filtered out here."
  (cl-remove-if (lambda (file)
                  (string-match-p llm-pick-test--test-regexp
                                  (file-name-nondirectory file)))
                (llm-pick-test--files llm-pick-test--module-regexp)))

(defun llm-pick-test--test-files ()
  "Return the test files, in directory order."
  (llm-pick-test--files llm-pick-test--test-regexp))

(defun llm-pick-test--features ()
  "Return the features to unload, derived from the module file names.
A new module file is picked up without touching this function."
  (cons 'llm-pick
        (mapcar (lambda (file) (intern (file-name-base file)))
                (llm-pick-test--module-files))))

(defun llm-pick-test--user-options ()
  "Return an alist of (SYMBOL . VALUE) for every `llm-pick' user option.
Only options the user really set are returned.  Customize marks them in
the `saved-value' property once the value is saved and in
`customized-value' when it is only set for the session.  An option which
still carries the default of an already loaded revision has neither mark,
so the reload is free to replace its value with the current default."
  (let (options)
    (mapatoms
     (lambda (symbol)
       (when (and (string-prefix-p "llm-pick-" (symbol-name symbol))
                  (boundp symbol)
                  (or (get symbol 'saved-value)
                      (get symbol 'customized-value)))
         (push (cons symbol (default-value symbol)) options))))
    options))

(defun llm-pick-test--stale-option-p (symbol)
  "Return non-nil when SYMBOL carries a default that a reload must refresh.
A stale option is a bound `llm-pick-' defcustom that the user never set,
so its value can only be the default of the revision that is loaded right
now.  Unbinding such a symbol lets the `defcustom' in the freshly loaded
source install the default it declares, because a `defcustom' installs
its default into a void symbol only.  Values the user set are excluded,
so a reload never throws away a running session's configuration.
This predicate looks at the `standard-value' property for presence only,
never evaluates it: that property survives the unload as a closure over
symbols that are void by then."
  (and (string-prefix-p "llm-pick-" (symbol-name symbol))
       (boundp symbol)
       (get symbol 'standard-value)
       (not (get symbol 'saved-value))
       (not (get symbol 'customized-value))))

(defun llm-pick-test--unbind-stale-options ()
  "Unbind every option selected by `llm-pick-test--stale-option-p'.
Called between the unload and the reload.  `unload-feature' already voids
the variables a module defined, so this is the fallback for a symbol it
could not reach, not the main road: it covers a defcustom whose module
never provided its feature and one whose load history entry is missing."
  (mapatoms (lambda (symbol)
              (when (llm-pick-test--stale-option-p symbol)
                (makunbound symbol)))))

(defun llm-pick-test--restore-user-options (options)
  "Restore the user option values recorded in OPTIONS."
  (dolist (option options)
    (when (boundp (car option))
      (set-default (car option) (cdr option)))))

(defun llm-pick-test--reload ()
  "Reload every module and test file from disk.
User options are kept across the unload/reload cycle, but only those the
user set: an option that still holds the default of the previous
revision is rebound to the default the source on disk declares.  The
project modules are loaded by explicit file name, so the .el sources of
the package win over any byte-compiled leftovers, whatever their
timestamps say: the suite always tests the code that is on disk right
now."
  (let ((options (llm-pick-test--user-options))
        ;; Do not reorder `load-suffixes' to put ".el" in front.  Emacs
        ;; appends the file name suffixes to a library name, so a leading
        ;; ".el" also matches the compressed sources of Emacs itself: the
        ;; first lazily required built-in would then be read from .el.gz
        ;; instead of .elc, and reading a .gz needs `jka-compr', whose own
        ;; implementation ships as .el.gz too.  "Recursive load:
        ;; jka-compr.el.gz" is the result.  The project modules do not
        ;; need the preference, `llm-pick-test--reload' loads them by
        ;; explicit file name below.
        ;;
        ;; `load-prefer-newer' stays nil: the reload has no stake in
        ;; newer source files, and leaving it at the default keeps a user
        ;; setting of t from reaching the libraries loaded below.
        (load-prefer-newer nil))
    (ert-delete-all-tests)
    (unwind-protect
        (progn
          (dolist (feature (reverse (llm-pick-test--features)))
            (when (featurep feature)
              (unload-feature feature t)))
          ;; `defcustom' installs its default only into a void symbol, so
          ;; the defaults of the previous revision have to go before the
          ;; sources are loaded again.
          (llm-pick-test--unbind-stale-options)
          (dolist (symbol llm-pick-test--reset-vars)
            (when (boundp symbol)
              (makunbound symbol)))
          (load-file (expand-file-name "llm-pick.el" llm-pick-test--root))
          (dolist (file (llm-pick-test--module-files))
            (load-file file)))
      (llm-pick-test--restore-user-options options))
    (dolist (file (llm-pick-test--test-files))
      (load-file file))))

(defun llm-pick-test--register-sources ()
  "Register the sources used by the test suite.

The snapshots live in `llm-pick-fixture-test' as inline JSON, so this
function only needs to load that library, which registers the sources
themselves."
  (require 'llm-pick-fixture-test)
  nil)

(defun llm-pick-test-run ()
  "Reload the modules and run the whole `llm-pick' test suite."
  (interactive)
  (let ((dir default-directory))
    (llm-pick-test--reload)
    ;; The built-in sources read their service only, so the suite registers
    ;; the snapshot-backed ones before anything runs.
    (llm-pick-test--register-sources)
    ;; The suite must not open a socket.  Every test that collects binds
    ;; `llm-pick-source-offline' itself and this binding is the net under the test
    ;; that forgets: a run that reaches a service is slow, is not
    ;; reproducible, and blocked this command outright once.
    (let ((llm-pick-source-offline t))
      (when (get-buffer "*ert*")
        (kill-buffer "*ert*"))
      (let ((default-directory dir))
        (if noninteractive
            (ert-run-tests-batch-and-exit "llm-pick-")
          (ert "llm-pick-")))
      ;; The fixture descriptors registered above survive the run and would
      ;; pollute the user session, so reload the built-in registrations.
      (load (expand-file-name "lisp/llm-pick-source.el" llm-pick-test--root) nil t))))

(provide 'llm-pick-test)

;;; llm-pick-test.el ends here
