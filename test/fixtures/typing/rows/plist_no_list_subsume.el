;; Bare list does not subsume to plist (no structural evidence)
;; test: emacs-version 31.0

(defun list-to-plist (p)
  (declare (tart ((list (keyword | string))) -> (plist keyword string)))
  p)
