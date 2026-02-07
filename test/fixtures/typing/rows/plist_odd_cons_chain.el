;; Odd-length cons chain rejected for plist promotion (no value for last key)
;; test: emacs-version 31.0

(defun accept-plist (p)
  (declare (tart ((plist keyword string)) -> string))
  (plist-get p :name))

;; Single element: odd length, cannot form key-value pairs
(defun odd-chain ()
  (accept-plist (cons :name nil)))
