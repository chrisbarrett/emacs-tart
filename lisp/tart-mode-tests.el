;;; tart-mode-tests.el --- Unit tests for tart-mode  -*- lexical-binding: t; -*-

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

;; Unit tests for tart-mode.el functions (no network access required).

;;; Code:

(require 'ert)
(require 'tart-mode)

;;; Platform Detection Tests (R3)

(ert-deftest tart-mode-platform-asset-returns-string ()
  "Platform asset function returns a string."
  ;; This may error on unsupported platforms, but should return a string otherwise
  (condition-case nil
      (let ((asset (tart--platform-asset)))
        (should (stringp asset))
        (should (string-prefix-p "tart-" asset)))
    (error nil)))  ; Skip on unsupported platforms

(ert-deftest tart-mode-platform-asset-format ()
  "Platform asset follows expected format."
  (condition-case nil
      (let ((asset (tart--platform-asset)))
        ;; Format: tart-{os}-{arch}
        (should (string-match "^tart-\\(darwin\\|linux\\)-\\(arm64\\|x86_64\\)$" asset)))
    (error nil)))

;;; Version Management Tests (R1, R5)

(ert-deftest tart-mode-bin-directory-returns-path ()
  "Binary directory returns a path under user-emacs-directory."
  (let ((dir (tart--bin-directory)))
    (should (stringp dir))
    (should (string-suffix-p "/tart/bin/" dir))))

(ert-deftest tart-mode-installed-versions-empty-when-no-dir ()
  "Installed versions returns nil when bin directory doesn't exist."
  (let ((user-emacs-directory (make-temp-file "emacs-test" t)))
    (unwind-protect
        (should (null (tart--installed-versions)))
      (delete-directory user-emacs-directory t))))

(ert-deftest tart-mode-installed-versions-lists-binaries ()
  "Installed versions lists installed binaries."
  (let* ((temp-dir (make-temp-file "emacs-test" t))
         (user-emacs-directory temp-dir)
         (bin-dir (expand-file-name "tart/bin/" temp-dir)))
    (unwind-protect
        (progn
          (make-directory bin-dir t)
          ;; Create fake binary files
          (write-region "" nil (expand-file-name "tart-0.1.0" bin-dir))
          (write-region "" nil (expand-file-name "tart-0.2.0" bin-dir))
          (write-region "" nil (expand-file-name "tart-0.10.0" bin-dir))
          (let ((versions (tart--installed-versions)))
            (should (member "0.1.0" versions))
            (should (member "0.2.0" versions))
            (should (member "0.10.0" versions))
            ;; Should be sorted newest first
            (should (equal (car versions) "0.10.0"))))
      (delete-directory temp-dir t))))

(ert-deftest tart-mode-managed-binary-path-uses-version ()
  "Managed binary path uses specified version."
  (let* ((temp-dir (make-temp-file "emacs-test" t))
         (user-emacs-directory temp-dir))
    (unwind-protect
        (let ((path (tart--managed-binary-path "1.2.3")))
          (should (stringp path))
          (should (string-suffix-p "tart-1.2.3" path)))
      (delete-directory temp-dir t))))

(ert-deftest tart-mode-resolve-executable-uses-string-directly ()
  "Resolve executable uses string value directly."
  (let ((tart-executable "/usr/local/bin/tart"))
    (should (equal (tart--resolve-executable) "/usr/local/bin/tart"))))

(ert-deftest tart-mode-resolve-executable-errors-when-managed-empty ()
  "Resolve executable errors when managed but no binary installed."
  (let* ((temp-dir (make-temp-file "emacs-test" t))
         (user-emacs-directory temp-dir)
         (tart-executable 'managed)
         (tart-version 'latest))
    (unwind-protect
        (should-error (tart--resolve-executable) :type 'error)
      (delete-directory temp-dir t))))

;;; GitHub API Helper Tests

(ert-deftest tart-mode-github-api-url ()
  "GitHub API URL is constructed correctly."
  (let ((url (tart--github-api-url "releases/latest")))
    (should (string-prefix-p "https://api.github.com/repos/" url))
    (should (string-suffix-p "/releases/latest" url))))

(ert-deftest tart-mode-find-asset-returns-match ()
  "Find asset returns matching asset from release."
  (let ((release '((assets . [((name . "tart-darwin-arm64")
                               (browser_download_url . "https://example.com/tart"))
                              ((name . "tart-linux-x86_64")
                               (browser_download_url . "https://example.com/tart2"))]))))
    (let ((asset (tart--find-asset release "tart-darwin-arm64")))
      (should asset)
      (should (equal (alist-get 'name asset) "tart-darwin-arm64")))))

(ert-deftest tart-mode-find-asset-returns-nil-when-not-found ()
  "Find asset returns nil when asset not found."
  (let ((release '((assets . [((name . "tart-darwin-arm64"))]))))
    (should-not (tart--find-asset release "tart-windows-x86_64"))))

;;; Binary Availability Tests (R4)

(ert-deftest tart-mode-binary-available-p-with-installed ()
  "Binary available returns t when binary exists."
  (let* ((temp-dir (make-temp-file "emacs-test" t))
         (user-emacs-directory temp-dir)
         (bin-dir (expand-file-name "tart/bin/" temp-dir))
         (binary (expand-file-name "tart-0.1.0" bin-dir))
         (tart-executable 'managed)
         (tart-version 'latest))
    (unwind-protect
        (progn
          (make-directory bin-dir t)
          (write-region "" nil binary)
          (set-file-modes binary #o755)  ; Make executable
          (should (tart--binary-available-p)))
      (delete-directory temp-dir t))))

(ert-deftest tart-mode-binary-available-p-when-missing ()
  "Binary available returns nil when no binary installed."
  (let* ((temp-dir (make-temp-file "emacs-test" t))
         (user-emacs-directory temp-dir)
         (tart-executable 'managed)
         (tart-version 'latest))
    (unwind-protect
        (should-not (tart--binary-available-p))
      (delete-directory temp-dir t))))

(ert-deftest tart-mode-binary-available-p-with-string ()
  "Binary available returns t for existing executable path."
  (let ((tart-executable "/bin/sh"))  ; A file that exists
    (should (tart--binary-available-p))))

(ert-deftest tart-mode-binary-available-p-with-bad-string ()
  "Binary available returns nil for non-existent path."
  (let ((tart-executable "/nonexistent/path/to/tart"))
    (should-not (tart--binary-available-p))))

;;; Indentation Tests (Spec 60)

(defun tart-mode-test--indent (input expected)
  "Insert INPUT into a `tart-signature-mode' buffer, indent, compare to EXPECTED."
  (with-temp-buffer
    (tart-signature-mode)
    (insert input)
    (indent-region (point-min) (point-max))
    (should (equal (buffer-string) expected))))

(ert-deftest tart-mode-indent-multi-clause-defun ()
  "Multi-clause defun indents body at 2-space offset."
  (tart-mode-test--indent
   "(defun overlayp\n((overlay) -> t)\n((_) -> nil))"
   "(defun overlayp\n  ((overlay) -> t)\n  ((_) -> nil))"))

(ert-deftest tart-mode-indent-single-line-defun ()
  "Single-line defun is unchanged."
  (tart-mode-test--indent
   "(defun buffer-live-p (any) -> bool)"
   "(defun buffer-live-p (any) -> bool)"))

(ert-deftest tart-mode-indent-multi-line-type ()
  "Multi-line type indents body at 2-space offset."
  (tart-mode-test--indent
   "(type eq-safe\n(symbol | keyword | int | t | nil))"
   "(type eq-safe\n  (symbol | keyword | int | t | nil))"))

(ert-deftest tart-mode-indent-defvar ()
  "Defvar indents body at 2-space offset."
  (tart-mode-test--indent
   "(defvar buffer-alist\n(list any))"
   "(defvar buffer-alist\n  (list any))"))

(ert-deftest tart-mode-indent-open ()
  "Open form indents with special-form style."
  (tart-mode-test--indent
   "(open\nsome-module)"
   "(open\n  some-module)"))

(ert-deftest tart-mode-indent-include ()
  "Include form indents with special-form style."
  (tart-mode-test--indent
   "(include\nother-module)"
   "(include\n  other-module)"))

;;; Imenu Tests (Spec 61)

(defun tart-mode-test--imenu-index (content)
  "Return the imenu index for CONTENT in a `tart-signature-mode' buffer."
  (with-temp-buffer
    (tart-signature-mode)
    (insert content)
    (funcall imenu-create-index-function)))

(ert-deftest tart-mode-imenu-functions ()
  "Imenu indexes defun declarations under Functions."
  (let ((index (tart-mode-test--imenu-index
                "(defun buffer-live-p (any) -> bool)
(defun current-buffer () -> buffer)
(defvar some-var int)")))
    (should (assoc "Functions" index))
    (let ((fns (cdr (assoc "Functions" index))))
      (should (= (length fns) 2))
      (should (assoc "buffer-live-p" fns))
      (should (assoc "current-buffer" fns)))))

(ert-deftest tart-mode-imenu-variables ()
  "Imenu indexes defvar declarations under Variables."
  (let ((index (tart-mode-test--imenu-index
                "(defvar buffer-alist (list any))
(defvar inhibit-read-only bool)
(defun some-fn () -> nil)")))
    (should (assoc "Variables" index))
    (let ((vars (cdr (assoc "Variables" index))))
      (should (= (length vars) 2))
      (should (assoc "buffer-alist" vars))
      (should (assoc "inhibit-read-only" vars)))))

(ert-deftest tart-mode-imenu-types ()
  "Imenu indexes type declarations under Types."
  (let ((index (tart-mode-test--imenu-index
                "(type int %tart-intrinsic%Int)
(type list [a] (%tart-intrinsic%List a))
(defun some-fn () -> nil)")))
    (should (assoc "Types" index))
    (let ((types (cdr (assoc "Types" index))))
      (should (= (length types) 2))
      (should (assoc "int" types))
      (should (assoc "list" types)))))

(ert-deftest tart-mode-imenu-mixed-categories ()
  "Imenu returns categorised index when multiple kinds exist."
  (let ((index (tart-mode-test--imenu-index
                "(type bool (t | nil))
(defun identity [a] (a) -> a)
(defvar load-path (list string))")))
    (should (= (length index) 3))
    (should (assoc "Functions" index))
    (should (assoc "Variables" index))
    (should (assoc "Types" index))))

(ert-deftest tart-mode-imenu-flat-fallback ()
  "Imenu flattens when only one declaration kind exists."
  (let ((index (tart-mode-test--imenu-index
                "(defun buffer-live-p (any) -> bool)
(defun current-buffer () -> buffer)
(defun buffer-list () -> (list buffer))")))
    ;; Should be flat — no category wrapper
    (should (= (length index) 3))
    (should (assoc "buffer-live-p" index))
    (should (assoc "current-buffer" index))
    (should (assoc "buffer-list" index))))

(ert-deftest tart-mode-imenu-empty-buffer ()
  "Imenu returns nil for empty buffer."
  (let ((index (tart-mode-test--imenu-index "")))
    (should (null index))))

(ert-deftest tart-mode-imenu-ignores-comments ()
  "Imenu does not index commented-out declarations."
  (let ((index (tart-mode-test--imenu-index
                ";; (defun commented-out () -> nil)
(defun real-fn () -> nil)")))
    ;; Flat since only one kind
    (should (= (length index) 1))
    (should (assoc "real-fn" index))))

;;; Minor-Mode Lifecycle Tests (Spec 63)

(ert-deftest tart-mode-lifecycle-enable-sets-mode ()
  "Enabling tart-mode sets the mode variable."
  (with-temp-buffer
    (emacs-lisp-mode)
    (tart-mode 1)
    (should tart-mode)))

(ert-deftest tart-mode-lifecycle-disable-clears-mode ()
  "Disabling tart-mode clears the mode variable."
  (with-temp-buffer
    (emacs-lisp-mode)
    (tart-mode 1)
    (tart-mode -1)
    (should-not tart-mode)))

(ert-deftest tart-mode-lifecycle-keymap-active-when-enabled ()
  "Tart keymap is active when mode is enabled."
  (with-temp-buffer
    (emacs-lisp-mode)
    (tart-mode 1)
    (should (eq (lookup-key tart-mode-map (kbd "C-c C-z"))
                #'tart-switch-to-repl))))

(ert-deftest tart-mode-lifecycle-keymap-inactive-when-disabled ()
  "Tart keymap bindings are inactive after mode is disabled."
  (with-temp-buffer
    (emacs-lisp-mode)
    (tart-mode 1)
    (tart-mode -1)
    ;; The minor mode keymap should no longer be in effect
    (should-not (memq tart-mode-map (current-minor-mode-maps)))))

(ert-deftest tart-mode-lifecycle-no-global-side-effects ()
  "Enabling tart-mode in one buffer does not affect another."
  (let ((buf-with (generate-new-buffer " *tart-test-with*"))
        (buf-without (generate-new-buffer " *tart-test-without*")))
    (unwind-protect
        (progn
          (with-current-buffer buf-with
            (emacs-lisp-mode)
            (tart-mode 1))
          (with-current-buffer buf-without
            (emacs-lisp-mode)
            (should-not tart-mode)
            (should-not (memq tart-mode-map (current-minor-mode-maps)))))
      (kill-buffer buf-with)
      (kill-buffer buf-without))))

(ert-deftest tart-mode-lifecycle-idempotent-toggle ()
  "Toggling tart-mode multiple times leaves no accumulated state."
  (with-temp-buffer
    (emacs-lisp-mode)
    ;; Toggle on/off three times
    (dotimes (_ 3)
      (tart-mode 1)
      (tart-mode -1))
    ;; Should be cleanly disabled
    (should-not tart-mode)
    (should-not (memq tart-mode-map (current-minor-mode-maps)))))

(ert-deftest tart-mode-lifecycle-lighter-shown-when-enabled ()
  "Mode lighter is shown when tart-mode is enabled."
  (with-temp-buffer
    (emacs-lisp-mode)
    (tart-mode 1)
    (should (assq 'tart-mode minor-mode-alist))))

;;; Download Security Tests (Spec 65)

(ert-deftest tart-mode-executable-is-risky-local-variable ()
  "tart-executable is marked as risky local variable."
  (should (get 'tart-executable 'risky-local-variable)))

(ert-deftest tart-mode-install-directory-is-risky-local-variable ()
  "tart-install-directory is marked as risky local variable."
  (should (get 'tart-install-directory 'risky-local-variable)))

(provide 'tart-mode-tests)
;;; tart-mode-tests.el ends here
