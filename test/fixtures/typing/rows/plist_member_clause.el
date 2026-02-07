;; Plist-member multi-clause dispatch with clause diagnostic
;; test: emacs-version 31.0
;;
;; When called with a proper (plist k v), the plist clause matches silently.
;; When called with a bare (list (k | v)), the fallback clause matches and
;; emits a warning.

;; Plist path: no warning
(defun test-plist-member (p)
  (declare (tart ((plist keyword string)) -> (plist keyword string)))
  (plist-member p :name))

;; Bare list path: warning
(defun test-list-member (xs)
  (declare (tart ((list (keyword | string))) -> (list (keyword | string))))
  (plist-member xs :name))
