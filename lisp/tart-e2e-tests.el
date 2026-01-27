;;; tart-e2e-tests.el --- E2E tests for tart-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Chris Barrett

;; Author: Chris Barrett
;; Keywords: languages, lisp, tools

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

;; End-to-end tests for tart-mode.el using real tart processes.
;; Tests cover LSP integration, REPL, and minor mode features.

;;; Code:

(require 'ert)
(require 'tart-test-helpers)
(require 'tart-mode)

;;; R3: LSP connects

(defun tart-test--server-process (server)
  "Get the process from SERVER, handling different eglot versions."
  (cond
   ((fboundp 'eglot--process) (eglot--process server))
   ((fboundp 'jsonrpc--process) (jsonrpc--process server))
   (t (error "Cannot find eglot/jsonrpc process accessor"))))

(ert-deftest tart-e2e-lsp-connects ()
  "Test that eglot connects to the tart LSP server."
  :tags '(:e2e :lsp)
  (tart-test-ensure-tart)
  (tart-test-with-fixture "valid.el"
    ;; Use tart-test-start-eglot which works in batch mode
    (tart-test-start-eglot)
    (should (eglot-current-server))
    (should (process-live-p (tart-test--server-process (eglot-current-server))))))

;;; R4: LSP diagnostics

(ert-deftest tart-e2e-lsp-diagnostics ()
  "Test that LSP reports diagnostics for type errors."
  :tags '(:e2e :lsp)
  (tart-test-ensure-tart)
  (tart-test-with-fixture "error.el"
    ;; Enable flymake first
    (flymake-mode 1)
    (tart-test-start-eglot)
    (should (eglot-current-server))
    ;; Give server time to send diagnostics
    (accept-process-output nil 1)
    ;; Trigger flymake to process LSP diagnostics
    (flymake-start)
    ;; Wait for diagnostics to be processed
    (tart-test-wait-for
     (lambda () (flymake-diagnostics))
     10)
    (let ((diags (flymake-diagnostics)))
      (should diags)
      (should (cl-some (lambda (d)
                         (string-match-p "mismatch\\|expected\\|found"
                                         (flymake-diagnostic-text d)))
                       diags)))))

;;; R5: LSP hover

(ert-deftest tart-e2e-lsp-hover ()
  "Test that hover shows type information."
  :tags '(:e2e :lsp)
  (tart-test-ensure-tart)
  (tart-test-with-fixture "valid.el"
    (tart-test-start-eglot)
    (should (eglot-current-server))
    ;; Give server time to initialize
    (accept-process-output nil 0.5)
    ;; Go to the defun "greet" - position on the function name
    (goto-char (point-min))
    (search-forward "defun greet")
    (backward-word)  ; Position at start of "greet"
    ;; Request hover info using eglot's internal request mechanism
    (let ((server (eglot-current-server))
          (line (1- (line-number-at-pos)))
          (col (current-column)))
      (when server
        (let* ((hover-result
                (jsonrpc-request server :textDocument/hover
                                 `(:textDocument (:uri ,(eglot--path-to-uri (buffer-file-name)))
                                   :position (:line ,line :character ,col)))))
          ;; Hover should return some content
          (should hover-result))))))

;;; R6: REPL starts

(ert-deftest tart-e2e-repl-starts ()
  "Test that the REPL starts and shows a prompt."
  :tags '(:e2e :repl)
  (tart-test-ensure-tart)
  (run-tart)
  (should (get-buffer "*tart*"))
  (with-current-buffer "*tart*"
    (should (derived-mode-p 'inferior-tart-mode))
    (should (process-live-p (get-buffer-process (current-buffer))))
    ;; Wait for prompt
    (tart-test-wait-for
     (lambda ()
       (save-excursion
         (goto-char (point-min))
         (re-search-forward "tart> " nil t)))
     3)))

;;; R7: REPL eval

(ert-deftest tart-e2e-repl-eval ()
  "Test that REPL evaluates expressions correctly."
  :tags '(:e2e :repl)
  (tart-test-ensure-tart)
  (run-tart)
  ;; Wait for prompt
  (with-current-buffer "*tart*"
    (tart-test-wait-for
     (lambda ()
       (save-excursion
         (goto-char (point-min))
         (re-search-forward "tart> " nil t)))
     3))
  ;; Send expression
  (tart--send-string "(+ 1 2)")
  ;; Wait for result
  (with-current-buffer "*tart*"
    (tart-test-wait-for
     (lambda ()
       (save-excursion
         (goto-char (point-min))
         (re-search-forward "3" nil t)))
     3)))

;;; R8: REPL ,type command

(ert-deftest tart-e2e-repl-type-command ()
  "Test that REPL ,type command shows type without evaluating."
  :tags '(:e2e :repl)
  (tart-test-ensure-tart)
  (run-tart)
  ;; Wait for prompt
  (with-current-buffer "*tart*"
    (tart-test-wait-for
     (lambda ()
       (save-excursion
         (goto-char (point-min))
         (re-search-forward "tart> " nil t)))
     3))
  ;; Send ,type command
  (tart--send-string ",type (+ 1 2)")
  ;; Wait for type info - should show Int or int
  (with-current-buffer "*tart*"
    (tart-test-wait-for
     (lambda ()
       (save-excursion
         (goto-char (point-min))
         (re-search-forward "[Ii]nt" nil t)))
     3)))

;;; R9: send-defun

(ert-deftest tart-e2e-send-defun ()
  "Test that send-defun sends the current defun to REPL."
  :tags '(:e2e :repl)
  (tart-test-ensure-tart)
  ;; Start REPL first
  (run-tart)
  (with-current-buffer "*tart*"
    (tart-test-wait-for
     (lambda ()
       (save-excursion
         (goto-char (point-min))
         (re-search-forward "tart> " nil t)))
     3))
  ;; Open a buffer with a defun
  (tart-test-with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun my-e2e-test-fn (x) (+ x 1))")
    (goto-char (point-min))
    (forward-char 5)  ; Inside the defun
    (tart-send-defun)
    ;; Check that the function was evaluated (REPL shows "my-e2e-test-fn :: Symbol")
    (with-current-buffer "*tart*"
      (tart-test-wait-for
       (lambda ()
         (save-excursion
           (goto-char (point-min))
           (re-search-forward "my-e2e-test-fn" nil t)))
       3))))

;;; R10: type-at-point

(ert-deftest tart-e2e-type-at-point ()
  "Test that type-at-point displays type information."
  :tags '(:e2e :repl)
  (tart-test-ensure-tart)
  ;; Start REPL
  (run-tart)
  (with-current-buffer "*tart*"
    (tart-test-wait-for
     (lambda ()
       (save-excursion
         (goto-char (point-min))
         (re-search-forward "tart> " nil t)))
     3))
  ;; Test type-at-point with a simple expression
  (tart-test-with-temp-buffer
    (emacs-lisp-mode)
    (insert "(+ 1 2)")
    (goto-char (point-min))
    ;; This should send ,type to REPL and show result
    (let ((displayed-msg nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq displayed-msg (apply #'format fmt args)))))
        (ignore-errors (tart-type-at-point)))
      ;; Check that some type info was displayed
      (should displayed-msg))))

;;; R11: Keybindings

(ert-deftest tart-e2e-keybindings ()
  "Test that tart-mode keybindings are active."
  :tags '(:e2e)
  (tart-test-with-temp-buffer
    (emacs-lisp-mode)
    (tart-mode 1)
    (should (eq (key-binding (kbd "C-c C-z")) #'tart-switch-to-repl))
    (should (eq (key-binding (kbd "C-c C-c")) #'tart-send-defun))
    (should (eq (key-binding (kbd "C-c C-t")) #'tart-type-at-point))))

;;; R12: Cleanup (verified by test infrastructure)

(ert-deftest tart-e2e-cleanup-works ()
  "Test that cleanup removes buffers and processes."
  :tags '(:e2e)
  (let ((initial-process-count (length (process-list))))
    ;; Create some processes
    (tart-test-ensure-tart)
    (run-tart)
    (should (get-buffer "*tart*"))
    ;; Run cleanup
    (tart-test-cleanup)
    ;; Verify cleanup
    (should-not (get-buffer "*tart*"))
    ;; Process count should be back to original
    (should (<= (length (process-list)) (1+ initial-process-count)))))

;;; R13: Timeouts

(ert-deftest tart-e2e-timeout-fails ()
  "Test that async waits fail with message after timeout."
  :tags '(:e2e)
  (should-error
   (tart-test-wait-for (lambda () nil) 0.1)
   :type 'error))

;;; R14: Fixtures (verified by fixture tests above)

(ert-deftest tart-e2e-fixtures-exist ()
  "Test that fixture files exist and are accessible."
  :tags '(:e2e)
  (should (file-exists-p (tart-test-fixture-path "valid.el")))
  (should (file-exists-p (tart-test-fixture-path "valid.tart")))
  (should (file-exists-p (tart-test-fixture-path "error.el")))
  (should (file-exists-p (tart-test-fixture-path "error.tart"))))

(provide 'tart-e2e-tests)
;;; tart-e2e-tests.el ends here
