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
