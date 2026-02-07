;;; tart-test-helpers.el --- Test utilities for tart E2E tests  -*- lexical-binding: t; -*-

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

;; Test utilities for tart-mode.el E2E tests.
;;
;; Provides macros and functions for:
;; - Temporary buffer setup with fixtures
;; - Async waiting for conditions
;; - Process cleanup
;; - Fixture file resolution

;;; Code:

(require 'ert)
(require 'project)

;;; Configuration

(defvar tart-test-fixture-dir
  (expand-file-name "test/fixtures/e2e/"
                    (file-name-directory
                     (directory-file-name
                      (file-name-directory load-file-name))))
  "Directory containing E2E test fixtures.")

(defvar tart-test-default-timeout 5.0
  "Default timeout in seconds for async operations.")

(defvar tart-test--cleanup-buffers nil
  "List of buffers to cleanup after each test.")

(defvar tart-test--cleanup-processes nil
  "List of processes to cleanup after each test.")

;;; Version Gating

(defmacro tart-test-skip-unless-version (major-version)
  "Skip the current test unless Emacs MAJOR-VERSION or later.
MAJOR-VERSION is an integer (e.g., 29, 30).  If the running Emacs
is older, the test is skipped with a descriptive message rather
than failing."
  (declare (indent 0) (debug (form)))
  `(when (< emacs-major-version ,major-version)
     (ert-skip (format "Requires Emacs %d+" ,major-version))))

;;; Fixture Resolution

(defun tart-test-fixture-path (name)
  "Return the full path to fixture file NAME.
NAME should be a relative path like \"valid.el\"."
  (let ((path (expand-file-name name tart-test-fixture-dir)))
    (unless (file-exists-p path)
      (error "Fixture file not found: %s" path))
    path))

;;; Executable Check

(defvar tart-test--project-root
  (file-name-directory
   (directory-file-name
    (file-name-directory load-file-name)))
  "Root directory of the tart project.")

(defun tart-test--find-tart-executable ()
  "Find the tart executable, checking build directory first."
  (let ((build-exe (expand-file-name "_build/default/bin/main.exe"
                                      tart-test--project-root)))
    (cond
     ((file-executable-p build-exe) build-exe)
     ((executable-find "tart"))
     (t nil))))

(defun tart-test-ensure-tart ()
  "Skip the test if the tart executable is not available.
Also sets `tart-executable' to the found path for use by tart-mode."
  (let ((exe (tart-test--find-tart-executable)))
    (unless exe
      (ert-skip "tart executable not found"))
    ;; Set tart-executable so tart-mode commands use it
    (setq tart-executable exe)))

;;; Async Waiting

(defun tart-test-wait-for (predicate &optional timeout)
  "Wait until PREDICATE returns non-nil, or fail after TIMEOUT seconds.
TIMEOUT defaults to `tart-test-default-timeout'.
Polls every 0.1 seconds."
  (let ((timeout (or timeout tart-test-default-timeout))
        (start (float-time))
        (poll-interval 0.1))
    (while (and (not (funcall predicate))
                (< (- (float-time) start) timeout))
      (accept-process-output nil poll-interval))
    (unless (funcall predicate)
      (error "Timeout after %.1f seconds waiting for condition" timeout))))

;;; Temporary Buffer Management

(defmacro tart-test-with-temp-buffer (&rest body)
  "Execute BODY in a temporary buffer with cleanup.
The buffer is added to the cleanup list for post-test cleanup."
  (declare (indent 0) (debug t))
  `(let ((buf (generate-new-buffer " *tart-test*")))
     (push buf tart-test--cleanup-buffers)
     (with-current-buffer buf
       ,@body)))

(defvar tart-test--current-temp-dir nil
  "Current test's temporary directory, for project mock.")

(defun tart-test--mock-project (_dir)
  "Return a mock transient project for the current temp directory."
  (when tart-test--current-temp-dir
    (cons 'transient tart-test--current-temp-dir)))

(defmacro tart-test-with-fixture (name &rest body)
  "Execute BODY with fixture NAME loaded in a buffer.
The fixture is copied to a temporary file so it can be modified.
The buffer visits the temp file so eglot can work with it.
A mock project is provided for eglot to work in batch mode."
  (declare (indent 1) (debug t))
  `(let* ((fixture-path (tart-test-fixture-path ,name))
          (temp-dir (make-temp-file "tart-test" t))
          (temp-file (expand-file-name (file-name-nondirectory fixture-path) temp-dir))
          ;; Copy fixture and its sibling .tart file if present
          (tart-sibling (concat (file-name-sans-extension fixture-path) ".tart"))
          (tart-temp (concat (file-name-sans-extension temp-file) ".tart"))
          ;; Mock project for eglot in batch mode
          (tart-test--current-temp-dir temp-dir)
          (project-find-functions (cons #'tart-test--mock-project project-find-functions)))
     (copy-file fixture-path temp-file t)
     (when (file-exists-p tart-sibling)
       (copy-file tart-sibling tart-temp t))
     (unwind-protect
         (let ((buf (find-file-noselect temp-file)))
           (push buf tart-test--cleanup-buffers)
           (with-current-buffer buf
             (emacs-lisp-mode)
             ,@body))
       ;; Shutdown eglot before deleting temp files
       (ignore-errors
         (when (and (fboundp 'eglot-current-server) (eglot-current-server))
           (eglot-shutdown (eglot-current-server))))
       ;; Cleanup temp files
       (setq tart-test--current-temp-dir nil)
       (ignore-errors (delete-directory temp-dir t)))))

;;; Eglot Helpers

(defun tart-test-start-eglot ()
  "Start eglot for the current buffer in batch mode.
Unlike `eglot-ensure', this works in batch mode by using `call-interactively'."
  (require 'eglot)
  (call-interactively #'eglot))

;;; Process Management

(defun tart-test--track-process (proc)
  "Add PROC to the cleanup list."
  (when (processp proc)
    (push proc tart-test--cleanup-processes)))

;;; Cleanup

(defun tart-test-cleanup ()
  "Kill all tracked buffers and processes.
Called automatically after each test."
  ;; Kill processes first
  (dolist (proc tart-test--cleanup-processes)
    (when (and proc (process-live-p proc))
      (ignore-errors
        (delete-process proc))))
  (setq tart-test--cleanup-processes nil)
  ;; Kill buffers
  (dolist (buf tart-test--cleanup-buffers)
    (when (buffer-live-p buf)
      (ignore-errors
        ;; Kill any associated process
        (when-let* ((proc (get-buffer-process buf)))
          (delete-process proc))
        (kill-buffer buf))))
  (setq tart-test--cleanup-buffers nil)
  ;; Kill any tart-related buffers
  (dolist (buf (buffer-list))
    (when (string-match-p "\\*tart\\*\\|\\*eglot" (buffer-name buf))
      (ignore-errors
        (when-let* ((proc (get-buffer-process buf)))
          (delete-process proc))
        (kill-buffer buf)))))

;; Register cleanup to run after each test
(add-hook 'ert-runner-after-test-hook #'tart-test-cleanup)

;; For non-batch mode, clean up via advice
(defun tart-test--cleanup-after-test (_test-or-result &rest _)
  "Clean up after test execution."
  (tart-test-cleanup))

(advice-add 'ert-run-test :after #'tart-test--cleanup-after-test)

(provide 'tart-test-helpers)
;;; tart-test-helpers.el ends here
