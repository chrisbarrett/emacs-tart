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

(defcustom tart-version nil
  "Tart version to install. nil = latest release.
This can be set in .dir-locals.el to pin a project to a specific version."
  :type '(choice (const :tag "Latest" nil)
                 (string :tag "Specific version"))
  :group 'tart
  :safe #'string-or-null-p)

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

(defcustom tart-repl-history-file
  (locate-user-emacs-file "tart-repl-history")
  "File to save REPL input history.
Set to nil to disable history persistence."
  :type '(choice (file :tag "History file")
                 (const :tag "No history file" nil))
  :group 'tart)

(defcustom tart-install-directory
  (locate-user-emacs-file "tart/bin/")
  "Directory for managed tart binary installations.
When `tart-executable' is `managed', binaries are downloaded to
this directory as tart-VERSION (e.g., tart-0.2.0)."
  :type 'directory
  :group 'tart)

;;; Inferior Tart Mode (REPL)

(defvar tart-error-regexp
  '(tart
    "^\\([^ \n]+\\):\\([0-9]+\\):\\([0-9]+\\)\\(?:-[0-9]+\\)?\\(?::[0-9]+\\)?: \\(?:error\\|warning\\): "
    1 2 3 2)
  "Compilation error regexp for tart output.
Matches file:line:col: error/warning: format.
The type is 2 (error) for both errors and warnings.")

(defvar inferior-tart-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map comint-mode-map)
    map)
  "Keymap for `inferior-tart-mode'.")

(defconst tart--prompt-regexp "^\\(?:tart\\|\\.\\.\\.\\) ?> "
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
  (when tart-repl-history-file
    (setq-local comint-input-ring-file-name tart-repl-history-file)
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
  (when (and tart-repl-history-file
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
  (expand-file-name tart-install-directory))

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
      (let ((files (directory-files bin-dir nil "^tart-[0-9]")))
        (sort (mapcar (lambda (f) (substring f 5)) files)
              (lambda (a b) (version< b a)))))))

(defun tart--managed-binary-path (&optional version)
  "Return the path to the managed binary for VERSION.
If VERSION is nil, uses `tart-version' or the latest installed version."
  (let* ((ver (or version tart-version (car (tart--installed-versions))))
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

(defun tart--github-api-url (endpoint)
  "Return the GitHub API URL for ENDPOINT."
  (format "https://api.github.com/repos/%s/%s" tart--github-repo endpoint))

(defun tart--github-request (endpoint callback)
  "Make async GitHub API request to ENDPOINT, calling CALLBACK with parsed JSON.
CALLBACK receives two arguments: the parsed JSON on success, or nil and an
error message string on failure."
  (let ((url (tart--github-api-url endpoint))
        (url-request-extra-headers
         `(("Accept" . "application/vnd.github+json")
           ("X-GitHub-Api-Version" . "2022-11-28")
           ,@(when-let* ((token (getenv "GITHUB_TOKEN")))
               `(("Authorization" . ,(concat "Bearer " token)))))))
    (url-retrieve
     url
     (lambda (status)
       (if-let* ((err (plist-get status :error)))
           (funcall callback nil (format "Network error: %s" (cadr err)))
         (goto-char url-http-end-of-headers)
         (condition-case err
             (let ((json (json-parse-buffer :object-type 'alist)))
               (if-let* ((message (alist-get 'message json)))
                   ;; GitHub API error response
                   (funcall callback nil
                            (if (string-match-p "rate limit" message)
                                "GitHub rate limit exceeded. Set GITHUB_TOKEN environment variable."
                              message))
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
Uses `tart-version' to determine which version to install, or
the latest release if nil.

The binary is installed to ~/.emacs.d/tart/bin/tart-VERSION
and will be used automatically when `tart-executable' is `managed'."
  (interactive)
  (let ((version tart-version)
        (asset-name (tart--platform-asset)))
    (message "Fetching release information...")
    (tart--github-request
     (if version
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

;; Register tart as an LSP server for emacs-lisp-mode
(add-to-list 'eglot-server-programs
             '(emacs-lisp-mode . tart--eglot-server-program))

(provide 'tart-mode)
;;; tart-mode.el ends here
