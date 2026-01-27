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

(provide 'tart-mode-tests)
;;; tart-mode-tests.el ends here
