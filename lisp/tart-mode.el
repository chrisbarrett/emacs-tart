;;; tart-mode.el --- Development tools for the Tart type checker  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Chris Barrett

;; Author: Chris Barrett
;; Keywords: languages, lisp, tools
;; Package-Requires: ((emacs "29.1"))
;; Version: 0.1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Development tools for the Tart type system for Emacs Lisp.  This package
;; provides:
;;
;; - LSP integration via eglot for type checking and hover
;; - REPL interaction (inferior-tart-mode)
;; - Minor mode with keybindings for REPL and type inspection
;;
;; Quick setup:
;;
;;   (require 'tart-mode)
;;   (add-hook 'emacs-lisp-mode-hook #'tart-eglot-ensure)
;;
;; Or manually start eglot in any elisp buffer that has a sibling .tart file:
;;
;;   M-x eglot
;;
;; Customization:
;;
;;   M-x customize-group RET tart RET
;;
;; See also `tart.el' for runtime type annotation macros.

;;; Code:

(require 'cl-lib)
(require 'comint)
(require 'compile)
(require 'eglot)
(require 'url)
(require 'xdg)

;;; Customization

(defgroup tart nil
  "Emacs integration for the Tart type checker."
  :group 'languages
  :prefix "tart-")

(defcustom tart-executable 'managed
  "Path to tart binary, or `managed' for automatic installation.
When `managed', uses downloaded binary from `tart-install-binary'.
When a string, use directly (absolute path or command on `exec-path')."
  :type '(choice (const :tag "Managed by tart-mode" managed)
                 (string :tag "Custom path"))
  :group 'tart)

(defcustom tart-version 'latest
  "Tart version to install.
When `latest', installs the most recent GitHub release.
When a version string (e.g., \"0.2.0\"), installs that specific version.
This can be set in .dir-locals.el to pin a project to a specific version."
  :type '(choice (const :tag "Latest" latest)
                 (string :tag "Specific version"))
  :group 'tart
  :safe (lambda (v) (or (eq v 'latest) (stringp v))))

(defcustom tart-lsp-args nil
  "Additional arguments to pass to `tart lsp'.
These are appended after the `lsp' subcommand."
  :type '(repeat string)
  :group 'tart)

(defcustom tart-repl-args nil
  "Additional arguments to pass to `tart repl'.
These are appended after the `repl' subcommand."
  :type '(repeat string)
  :group 'tart)

(defcustom tart-directory-style 'xdg
  "Where to store tart files (binaries, history).
When `xdg', uses XDG base directories:
  - Binaries in $XDG_DATA_HOME/tart/bin/ (~/.local/share/tart/bin/)
  - History in $XDG_STATE_HOME/tart/repl-history (~/.local/state/tart/)
When `emacs', uses Emacs user directory:
  - Binaries in ~/.emacs.d/tart/bin/
  - History in ~/.emacs.d/tart-repl-history"
  :type '(choice (const :tag "XDG base directories" xdg)
                 (const :tag "Emacs user directory" emacs))
  :group 'tart)

(defun tart--default-install-directory ()
  "Return the default install directory based on `tart-directory-style'."
  (pcase tart-directory-style
    ('xdg (expand-file-name "tart/bin/" (xdg-data-home)))
    ('emacs (locate-user-emacs-file "tart/bin/"))))

(defun tart--default-history-file ()
  "Return the default history file path based on `tart-directory-style'."
  (pcase tart-directory-style
    ('xdg (expand-file-name "tart/repl-history" (xdg-state-home)))
    ('emacs (locate-user-emacs-file "tart-repl-history"))))

(defcustom tart-repl-history-file 'default
  "File to save REPL input history.
When `default', uses path based on `tart-directory-style'.
When a string, uses that path directly.
Set to nil to disable history persistence."
  :type '(choice (const :tag "Based on tart-directory-style" default)
                 (file :tag "Custom path")
                 (const :tag "No history file" nil))
  :group 'tart)

(defcustom tart-install-directory 'default
  "Directory for managed tart binary installations.
When `default', uses path based on `tart-directory-style'.
When a string, uses that directory directly.
Binaries are downloaded as tart-VERSION (e.g., tart-0.2.0)."
  :type '(choice (const :tag "Based on tart-directory-style" default)
                 (directory :tag "Custom directory"))
  :group 'tart)

(defcustom tart-setup-find-sibling-rules t
  "Whether to add find-sibling rules for .tart and .el files.
When non-nil, adds rules to `find-sibling-rules' so that
`find-sibling-file' can navigate between .tart signature files
and their corresponding .el implementation files."
  :type 'boolean
  :group 'tart)

(defcustom tart-setup-eglot t
  "Whether to register tart as an eglot server.
When non-nil, adds tart to `eglot-server-programs' for both
`emacs-lisp-mode' and `tart-signature-mode', so that eglot can
use tart for type checking elisp files and signature files."
  :type 'boolean
  :group 'tart)

;;; Inferior Tart Mode (REPL)

(defvar tart-error-regexp
  `(tart
    ,(rx bol
         (group (+ (not (any " \n"))))      ; file
         ":" (group (+ digit))              ; line
         ":" (group (+ digit))              ; col
         (? "-" (+ digit))                  ; optional end col
         (? ":" (+ digit))                  ; optional second col
         ": " (or "error" "warning") ": ")
    1 2 3 2)
  "Compilation error regexp for tart output.
Matches file:line:col: error/warning: format.
The type is 2 (error) for both errors and warnings.")

(defvar inferior-tart-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map comint-mode-map)
    map)
  "Keymap for `inferior-tart-mode'.")

(defconst tart--prompt-regexp (rx bol (or "tart" "...") (? " ") "> ")
  "Regexp matching the tart REPL prompt.
Matches both the main prompt \"tart> \" and continuation \"... > \".")

(defvar inferior-tart-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Lisp-style syntax
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    ;; Comments
    (modify-syntax-entry ?\; "<" table)
    (modify-syntax-entry ?\n ">" table)
    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)
    ;; Symbol constituents
    (modify-syntax-entry ?- "_" table)
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?* "_" table)
    (modify-syntax-entry ?+ "_" table)
    (modify-syntax-entry ?/ "_" table)
    (modify-syntax-entry ?< "_" table)
    (modify-syntax-entry ?> "_" table)
    (modify-syntax-entry ?= "_" table)
    (modify-syntax-entry ?? "_" table)
    (modify-syntax-entry ?! "_" table)
    table)
  "Syntax table for `inferior-tart-mode'.")

(define-derived-mode inferior-tart-mode comint-mode "Inferior Tart"
  "Major mode for interacting with a tart REPL process.

Commands:
\\{inferior-tart-mode-map}"
  :syntax-table inferior-tart-mode-syntax-table
  (setq-local comint-prompt-regexp tart--prompt-regexp)
  (setq-local comint-prompt-read-only t)
  ;; History configuration
  (setq-local comint-input-ring-size 500)
  (when-let* ((history-file (pcase tart-repl-history-file
                              ('default (tart--default-history-file))
                              ((pred stringp) tart-repl-history-file))))
    (setq-local comint-input-ring-file-name history-file)
    (comint-read-input-ring t))
  ;; Font-lock for output
  (setq-local font-lock-defaults '(nil nil nil nil))
  ;; Enable compilation-mode style error parsing
  (add-to-list 'compilation-error-regexp-alist 'tart)
  (add-to-list 'compilation-error-regexp-alist-alist tart-error-regexp)
  (compilation-shell-minor-mode 1))

(defun tart--repl-buffer ()
  "Return the tart REPL buffer, or nil if none exists."
  (get-buffer "*tart*"))

(defun tart--repl-process ()
  "Return the tart REPL process, or nil if none."
  (when-let* ((buf (tart--repl-buffer)))
    (get-buffer-process buf)))

(defun tart--save-repl-history ()
  "Save REPL history to file."
  (when (and tart-repl-history-file  ; not nil
             (derived-mode-p 'inferior-tart-mode))
    (comint-write-input-ring)))

;;;###autoload
(defun run-tart ()
  "Start an inferior tart REPL process.
If a REPL is already running, switch to it."
  (interactive)
  (let* ((buffer (tart--repl-buffer))
         (proc (and buffer (get-buffer-process buffer))))
    (if (and proc (process-live-p proc))
        (pop-to-buffer buffer)
      ;; Start new process
      (let ((program (tart--resolve-executable))
            (args (cons "repl" tart-repl-args)))
        (with-current-buffer (apply #'make-comint "tart" program nil args)
          (inferior-tart-mode)
          (add-hook 'kill-buffer-hook #'tart--save-repl-history nil t))
        (pop-to-buffer "*tart*")))))

;;;###autoload
(defalias 'inferior-tart #'run-tart
  "Alias for `run-tart' for consistency with other inferior modes.")

;;; Send-to-REPL Commands

(defun tart--ensure-repl ()
  "Ensure a tart REPL process is running, returning the process.
Starts the REPL if not already running."
  (or (tart--repl-process)
      (progn
        (save-window-excursion (run-tart))
        (tart--repl-process))))

(defun tart--send-string (string)
  "Send STRING to the tart REPL process.
Ensures the REPL is running before sending."
  (let ((proc (tart--ensure-repl)))
    (comint-send-string proc string)
    ;; Ensure newline at end for evaluation
    (unless (string-suffix-p "\n" string)
      (comint-send-string proc "\n"))))

;;;###autoload
(defun tart-send-region (start end)
  "Send the region from START to END to the tart REPL."
  (interactive "r")
  (tart--send-string (buffer-substring-no-properties start end)))

;;;###autoload
(defun tart-send-defun ()
  "Send the defun at point to the tart REPL."
  (interactive)
  (save-excursion
    (end-of-defun)
    (let ((end (point)))
      (beginning-of-defun)
      (tart--send-string (buffer-substring-no-properties (point) end)))))

;;;###autoload
(defun tart-send-last-sexp ()
  "Send the sexp before point to the tart REPL."
  (interactive)
  (let ((end (point))
        (start (save-excursion
                 (backward-sexp)
                 (point))))
    (tart--send-string (buffer-substring-no-properties start end))))

;;;###autoload
(defun tart-send-buffer ()
  "Send the entire buffer to the tart REPL."
  (interactive)
  (tart--send-string (buffer-substring-no-properties (point-min) (point-max))))

;;;###autoload
(defun tart-switch-to-repl ()
  "Switch to the tart REPL buffer, starting one if needed."
  (interactive)
  (tart--ensure-repl)
  (pop-to-buffer (tart--repl-buffer)))

;;; Type/Expand Inspection Commands

(defvar tart--repl-response nil
  "Accumulated response from REPL command.")

(defvar tart--repl-waiting nil
  "Non-nil when waiting for REPL response.")

(defun tart--repl-output-filter (output)
  "Filter function to capture REPL output.
OUTPUT is the string received from the process."
  (when tart--repl-waiting
    (setq tart--repl-response (concat tart--repl-response output)))
  output)

(defun tart--extract-repl-result (output)
  "Extract the result from REPL OUTPUT, removing prompts."
  (let ((lines (split-string output "\n" t)))
    ;; Filter out prompt lines and empty lines
    (setq lines (cl-remove-if
                 (lambda (line)
                   (or (string-match-p tart--prompt-regexp line)
                       (string-empty-p (string-trim line))))
                 lines))
    (string-trim (string-join lines "\n"))))

(defun tart--send-repl-command (command)
  "Send COMMAND to REPL and return the response string.
Blocks until the REPL responds with a new prompt."
  (let ((proc (tart--ensure-repl))
        (buf (tart--repl-buffer)))
    (setq tart--repl-response ""
          tart--repl-waiting t)
    (with-current-buffer buf
      (add-hook 'comint-preoutput-filter-functions #'tart--repl-output-filter nil t))
    (unwind-protect
        (progn
          (comint-send-string proc command)
          (unless (string-suffix-p "\n" command)
            (comint-send-string proc "\n"))
          ;; Wait for response (prompt indicates completion)
          (with-timeout (5 (error "Timeout waiting for REPL response"))
            (while (not (string-match-p tart--prompt-regexp tart--repl-response))
              (accept-process-output proc 0.1)))
          (tart--extract-repl-result tart--repl-response))
      (setq tart--repl-waiting nil)
      (with-current-buffer buf
        (remove-hook 'comint-preoutput-filter-functions #'tart--repl-output-filter t)))))

(defun tart--sexp-at-point ()
  "Return the sexp at or before point as a string."
  (save-excursion
    (let ((bounds (bounds-of-thing-at-point 'sexp)))
      (if bounds
          (buffer-substring-no-properties (car bounds) (cdr bounds))
        ;; Fall back to sexp before point
        (let ((end (point))
              (start (save-excursion
                       (backward-sexp)
                       (point))))
          (buffer-substring-no-properties start end))))))

;;;###autoload
(defun tart-type-at-point ()
  "Show the type of the sexp at point in the echo area.
Sends `,type <sexp>' to the tart REPL and displays the result."
  (interactive)
  (let* ((sexp (tart--sexp-at-point))
         (result (tart--send-repl-command (concat ",type " sexp))))
    (message "%s" result)))

;;;###autoload
(defun tart-expand-at-point ()
  "Show the macro expansion of the sexp at point.
Sends `,expand <sexp>' to the tart REPL and displays the result."
  (interactive)
  (let* ((sexp (tart--sexp-at-point))
         (result (tart--send-repl-command (concat ",expand " sexp))))
    (message "%s" result)))

;;; Major Mode for .tart Files

(defvar tart-signature-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Paired delimiters
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    ;; Comments
    (modify-syntax-entry ?\; "<" table)
    (modify-syntax-entry ?\n ">" table)
    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)
    ;; Symbol constituents
    (modify-syntax-entry ?- "_" table)
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?* "_" table)
    (modify-syntax-entry ?+ "_" table)
    (modify-syntax-entry ?/ "_" table)
    (modify-syntax-entry ?< "_" table)
    (modify-syntax-entry ?> "_" table)
    (modify-syntax-entry ?= "_" table)
    (modify-syntax-entry ?? "_" table)
    (modify-syntax-entry ?! "_" table)
    (modify-syntax-entry ?| "_" table)
    (modify-syntax-entry ?& "_" table)
    (modify-syntax-entry ?: "_" table)
    table)
  "Syntax table for `tart-signature-mode'.
Modelled on `inferior-tart-mode-syntax-table' with additional
symbol constituents for `|' (union operator), `&' (`&optional',
`&rest', `&key'), and `:' (type bounds).")

(defvar tart-signature-mode-font-lock-keywords
  (let ((declaration-keywords '("defun" "defvar" "type" "open" "include")))
    `(;; R1: Declaration keywords
      (,(concat "(" (regexp-opt declaration-keywords t) "\\_>")
       (1 font-lock-keyword-face))

      ;; R2: Function name after defun
      ("(defun\\_>[ \t]+\\(\\(?:\\sw\\|\\s_\\)+\\)"
       (1 font-lock-function-name-face))

      ;; R2: Variable name after defvar
      ("(defvar\\_>[ \t]+\\(\\(?:\\sw\\|\\s_\\)+\\)"
       (1 font-lock-variable-name-face))

      ;; R2: Type name after type
      ("(type\\_>[ \t]+\\(\\(?:\\sw\\|\\s_\\)+\\)"
       (1 font-lock-type-face))

      ;; R5: Module name after open/include
      (,(concat "(" (regexp-opt '("open" "include") t)
                "\\_>[ \t]+\\(\\(?:\\sw\\|\\s_\\)+\\)")
       (2 font-lock-constant-face))

      ;; R3: Arrow operator
      ("\\_<->\\_>"
       (0 font-lock-keyword-face))

      ;; R4: Type variable quantifiers — variables inside [...]
      ("\\[\\([^]]*\\)\\]"
       (1 font-lock-variable-name-face))))
  "Font-lock keywords for `tart-signature-mode'.")

(defvar tart-signature-mode--indent-overrides
  '((defun   . defun)
    (defvar  . defun)
    (type    . defun)
    (open    . defun)
    (include . defun))
  "Alist of tart indentation overrides.
Each entry is (SYMBOL . METHOD) where METHOD is either `defun'
for 2-space body indentation, or an integer for `lisp-indent-specform'.")

(defun tart-signature-indent-function (indent-point state)
  "Indent function for `tart-signature-mode'.
Looks up `tart-signature-mode--indent-overrides' first, then
falls back to `lisp-indent-function' symbol properties."
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
             (not (looking-at "\\sw\\|\\s_")))
        ;; Car of form is not a symbol; use default alignment.
        (progn
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp))
              (progn (goto-char calculate-lisp-indent-last-sexp)
                     (beginning-of-line)
                     (parse-partial-sexp (point)
                                         calculate-lisp-indent-last-sexp
                                         0 t)))
          (backward-prefix-chars)
          (current-column))
      (let* ((function (buffer-substring (point)
                                         (progn (forward-sexp 1) (point))))
             (sym (intern-soft function))
             (method (or (cdr (assq sym tart-signature-mode--indent-overrides))
                         (and sym (function-get sym 'lisp-indent-function))
                         (and sym (get sym 'lisp-indent-hook)))))
        (cond ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`def" function)))
               (lisp-indent-defform state indent-point))
              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method indent-point state)))))))

(defvar tart-signature-mode-imenu-generic-expression
  `(("Functions" ,(rx bol (* space) "(defun" (+ space)
                      (group (+ (or (syntax word) (syntax symbol)))))
     1)
    ("Variables" ,(rx bol (* space) "(defvar" (+ space)
                      (group (+ (or (syntax word) (syntax symbol)))))
     1)
    ("Types" ,(rx bol (* space) "(type" (+ space)
                  (group (+ (or (syntax word) (syntax symbol)))))
     1))
  "Imenu generic expression for `tart-signature-mode'.
Indexes `defun', `defvar', and `type' declarations.")

(defun tart-signature-mode--imenu-create-index ()
  "Create imenu index for current `tart-signature-mode' buffer.
Flattens the index when all entries fall under a single category,
avoiding unnecessary nesting (e.g., most c-core files are all `defun')."
  (let ((index (imenu-default-create-index-function)))
    (if (and (= (length index) 1)
             (consp (car index))
             (listp (cdar index)))
        ;; Single category — flatten by stripping the category wrapper
        (cdar index)
      index)))

;;;###autoload
(define-derived-mode tart-signature-mode lisp-mode "Tart"
  "Major mode for editing Tart type signature files."
  :syntax-table tart-signature-mode-syntax-table
  (setq-local font-lock-defaults
              '(tart-signature-mode-font-lock-keywords
                nil nil nil nil))
  (setq-local lisp-indent-function #'tart-signature-indent-function)
  (setq-local imenu-generic-expression
              tart-signature-mode-imenu-generic-expression)
  (setq-local imenu-create-index-function
              #'tart-signature-mode--imenu-create-index))

;;;###autoload
(add-to-list 'auto-mode-alist `(,(rx ".tart" eos) . tart-signature-mode))

;;; Minor Mode

(defvar tart-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-z") #'tart-switch-to-repl)
    (define-key map (kbd "C-c C-c") #'tart-send-defun)
    (define-key map (kbd "C-c C-r") #'tart-send-region)
    (define-key map (kbd "C-c C-e") #'tart-send-last-sexp)
    (define-key map (kbd "C-c C-b") #'tart-send-buffer)
    (define-key map (kbd "C-c C-t") #'tart-type-at-point)
    (define-key map (kbd "C-c C-x") #'tart-expand-at-point)
    map)
  "Keymap for `tart-mode'.")

;;;###autoload
(define-minor-mode tart-mode
  "Minor mode for Tart type checker integration.

Provides keybindings for REPL interaction and type inspection.

\\{tart-mode-map}"
  :lighter " Tart"
  :keymap tart-mode-map
  :group 'tart)

;;; Binary Management

(defconst tart--github-repo "chrisbarrett/emacs-tart"
  "GitHub repository for tart releases.")

(defun tart--bin-directory ()
  "Return the directory for managed tart binaries."
  (expand-file-name
   (if (eq tart-install-directory 'default)
       (tart--default-install-directory)
     tart-install-directory)))

(defun tart--platform-asset ()
  "Return the asset name for the current platform.
Returns a string like \"tart-darwin-arm64\" or signals an error
if the platform is not supported."
  (let* ((os (pcase system-type
               ('darwin "darwin")
               ('gnu/linux "linux")
               (_ nil)))
         (arch (pcase (car (split-string system-configuration "-"))
                 ((or "aarch64" "arm64") "arm64")
                 ("x86_64" "x86_64")
                 (_ nil))))
    (unless os
      (error "Unsupported operating system: %s (supported: darwin, gnu/linux)" system-type))
    (unless arch
      (error "Unsupported architecture: %s (supported: arm64, x86_64)"
             (car (split-string system-configuration "-"))))
    (format "tart-%s-%s" os arch)))

(defun tart--installed-versions ()
  "Return a list of installed tart versions, newest first.
Each element is a version string like \"0.2.0\"."
  (let ((bin-dir (tart--bin-directory)))
    (when (file-directory-p bin-dir)
      (let ((files (directory-files bin-dir nil (rx bos "tart-" digit))))
        (sort (mapcar (lambda (f) (substring f 5)) files)
              (lambda (a b) (version< b a)))))))

(defun tart--managed-binary-path (&optional version)
  "Return the path to the managed binary for VERSION.
If VERSION is nil or `latest', uses the latest installed version.
If `tart-version' is a string, uses that specific version."
  (let* ((requested (or version tart-version))
         (ver (if (stringp requested)
                  requested
                (car (tart--installed-versions))))
         (bin-dir (tart--bin-directory)))
    (when ver
      (expand-file-name (concat "tart-" ver) bin-dir))))

(defun tart--resolve-executable ()
  "Resolve `tart-executable' to an actual binary path.
When `tart-executable' is `managed', returns the path to the
downloaded binary (per `tart-version' or latest installed).
When a string, returns it directly for absolute paths, or
searches `exec-path' for relative paths."
  (pcase tart-executable
    ('managed
     (or (tart--managed-binary-path)
         (error "No tart binary installed. Run M-x tart-install-binary")))
    ((pred stringp)
     (if (file-name-absolute-p tart-executable)
         tart-executable
       (or (executable-find tart-executable)
           (error "Cannot find tart executable: %s" tart-executable))))
    (_
     (error "Invalid tart-executable value: %S" tart-executable))))

;;; Binary Installation

(defvar url-http-end-of-headers)  ; defined by url.el

(defun tart--github-request (endpoint callback)
  "Make async GitHub API request to ENDPOINT, calling CALLBACK with parsed JSON.
CALLBACK receives two arguments: the parsed JSON on success, or nil and an
error message string on failure."
  (let ((url (format "https://api.github.com/repos/%s/%s" tart--github-repo endpoint))
        (url-request-extra-headers '(("Accept" . "application/vnd.github+json")
                                     ("X-GitHub-Api-Version" . "2022-11-28"))))
    (url-retrieve
     url
     (lambda (status)
       (if-let* ((err (plist-get status :error)))
           (funcall callback nil (format "Network error: %s" (cadr err)))
         (goto-char url-http-end-of-headers)
         (condition-case err
             (let ((json (json-parse-buffer :object-type 'alist)))
               (if-let* ((message (alist-get 'message json)))
                   (funcall callback nil message) ; Show GitHub API error
                 (funcall callback json nil)))
           (error (funcall callback nil (format "JSON parse error: %s" err))))))
     nil t t)))

(defun tart--find-asset (release asset-name)
  "Find ASSET-NAME in RELEASE's assets list.
Returns the asset alist or nil if not found."
  (let ((assets (alist-get 'assets release)))
    (cl-find-if (lambda (a) (equal (alist-get 'name a) asset-name))
                assets)))

(defun tart--download-binary (url dest callback)
  "Download binary from URL to DEST file, calling CALLBACK when done.
CALLBACK receives two arguments: DEST path on success, or nil and error message."
  (message "Downloading tart binary...")
  (let ((url-request-extra-headers
         `(("Accept" . "application/octet-stream")
           ,@(when-let* ((token (getenv "GITHUB_TOKEN")))
               `(("Authorization" . ,(concat "Bearer " token)))))))
    (url-retrieve
     url
     (lambda (status)
       (if-let* ((err (plist-get status :error)))
           (funcall callback nil (format "Download failed: %s" (cadr err)))
         ;; Write binary data to file
         (goto-char url-http-end-of-headers)
         (let ((coding-system-for-write 'binary))
           (condition-case err
               (progn
                 (make-directory (file-name-directory dest) t)
                 (write-region (point) (point-max) dest nil 'silent)
                 (set-file-modes dest #o755)
                 (message "Downloaded tart to %s" dest)
                 (funcall callback dest nil))
             (error (funcall callback nil (format "Write error: %s" err)))))))
     nil t t)))

;;;###autoload
(defun tart-install-binary ()
  "Download and install the tart binary from GitHub releases.
Uses `tart-version' to determine which version to install.
When `latest', installs the most recent release.

The binary is installed to ~/.emacs.d/tart/bin/tart-VERSION
and will be used automatically when `tart-executable' is `managed'."
  (interactive)
  (let ((version tart-version)
        (asset-name (tart--platform-asset)))
    (message "Fetching release information...")
    (tart--github-request
     (if (stringp version)
         (format "releases/tags/v%s" version)
       "releases/latest")
     (lambda (release err)
       (if err
           (user-error "Failed to fetch release: %s" err)
         (let* ((tag (alist-get 'tag_name release))
                (ver (if (string-prefix-p "v" tag) (substring tag 1) tag))
                (asset (tart--find-asset release asset-name))
                (dest (expand-file-name (concat "tart-" ver) (tart--bin-directory))))
           (unless asset
             (user-error "No binary available for %s. Supported: darwin-arm64, darwin-x86_64, linux-arm64, linux-x86_64"
                         asset-name))
           (if (file-exists-p dest)
               (message "tart %s already installed at %s" ver dest)
             (tart--download-binary
              (alist-get 'browser_download_url asset)
              dest
              (lambda (_path err)
                (if err
                    (user-error "Installation failed: %s" err)
                  (message "Successfully installed tart %s" ver)))))))))))

;;; Eglot Integration

(defun tart--eglot-server-program (_interactive)
  "Return the tart LSP server command.
INTERACTIVE is ignored but required by eglot's server program interface."
  `(,(tart--resolve-executable) "lsp" ,@tart-lsp-args))

(defun tart--has-sibling-tart-file-p ()
  "Return non-nil if current buffer's file has a sibling .tart file."
  (when-let* ((file (buffer-file-name)))
    (file-exists-p (concat (file-name-sans-extension file) ".tart"))))

(defun tart--binary-available-p ()
  "Return non-nil if a tart binary is available.
Checks based on `tart-executable' setting:
- If `managed': checks for installed binary in bin directory
- If string: checks file exists or is on exec-path"
  (condition-case nil
      (let ((path (tart--resolve-executable)))
        (and path (file-executable-p path)))
    (error nil)))

;;;###autoload
(defun tart-eglot ()
  "Start eglot for tart, prompting to install if binary missing.
When the current buffer has a sibling .tart file, starts eglot for
type checking.  If no tart binary is available and `tart-executable'
is `managed', prompts to install one first.

Intended for use in `emacs-lisp-mode-hook' or interactively."
  (interactive)
  (when (tart--has-sibling-tart-file-p)
    (unless (tart--binary-available-p)
      (if (eq tart-executable 'managed)
          (if (y-or-n-p "Tart binary not found. Install now? ")
              (progn
                (tart-install-binary)
                ;; Wait briefly for async download to start, then let user know
                (message "Download started. Run M-x tart-eglot again after installation completes."))
            (user-error "Tart binary required for type checking"))
        (user-error "Tart binary not found: %s" tart-executable)))
    (when (tart--binary-available-p)
      (eglot-ensure))))

;;;###autoload
(defun tart-eglot-ensure ()
  "Start eglot if the current buffer has a sibling .tart file.
This is a simpler version of `tart-eglot' that does not prompt for
installation.  Intended for use in `emacs-lisp-mode-hook' when you
have already installed the binary."
  (when (and (tart--has-sibling-tart-file-p)
             (tart--binary-available-p))
    (eglot-ensure)))

;; Register tart as an LSP server for emacs-lisp-mode and tart-signature-mode.
;; Using a multi-mode entry means eglot shares a single server connection when
;; both .el and .tart buffers belong to the same project (Spec 62, R3).
(when tart-setup-eglot
  (add-to-list 'eglot-server-programs
               '((emacs-lisp-mode tart-signature-mode)
                 . tart--eglot-server-program)))

;;; Find-sibling Integration

(when tart-setup-find-sibling-rules
  (add-to-list 'find-sibling-rules
               `(,(rx (group (+? anything)) ".tart" eos) "\\1.el"))
  (add-to-list 'find-sibling-rules
               `(,(rx (group (+? anything)) ".el" eos) "\\1.tart")))

(provide 'tart-mode)
;;; tart-mode.el ends here
