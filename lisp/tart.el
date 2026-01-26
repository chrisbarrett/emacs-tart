;;; tart.el --- Emacs integration for the Tart type checker  -*- lexical-binding: t; -*-

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

;; Tart is a static type system for Emacs Lisp.  This package provides
;; integration with the tart type checker via eglot (LSP).
;;
;; Quick setup:
;;
;;   (require 'tart)
;;   (add-hook 'emacs-lisp-mode-hook #'tart-eglot-ensure)
;;
;; Or manually start eglot in any elisp buffer that has a sibling .tart file:
;;
;;   M-x eglot
;;
;; Customization:
;;
;;   M-x customize-group RET tart RET

;;; Code:

(require 'comint)
(require 'eglot)

;;; Customization

(defgroup tart nil
  "Emacs integration for the Tart type checker."
  :group 'languages
  :prefix "tart-")

(defcustom tart-executable "tart"
  "Path to the tart executable.
Can be an absolute path or a command name to be found on `exec-path'."
  :type 'string
  :group 'tart)

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

;;; Inferior Tart Mode (REPL)

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
  (setq-local font-lock-defaults '(nil nil nil nil)))

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
      (let ((program tart-executable)
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

;;; Eglot Integration

(defun tart--eglot-server-program (_interactive)
  "Return the tart LSP server command.
INTERACTIVE is ignored but required by eglot's server program interface."
  `(,tart-executable "lsp" ,@tart-lsp-args))

(defun tart--has-sibling-tart-file-p ()
  "Return non-nil if current buffer's file has a sibling .tart file."
  (when-let* ((file (buffer-file-name)))
    (file-exists-p (concat (file-name-sans-extension file) ".tart"))))

(defun tart-eglot-ensure ()
  "Start eglot if the current buffer has a sibling .tart file.
Intended for use in `emacs-lisp-mode-hook'."
  (when (tart--has-sibling-tart-file-p)
    (eglot-ensure)))

;; Register tart as an LSP server for emacs-lisp-mode
(add-to-list 'eglot-server-programs
             '(emacs-lisp-mode . tart--eglot-server-program))

(provide 'tart)
;;; tart.el ends here
