;; Cons chain with wrong alternation rejected for plist promotion
;; test: emacs-version 31.0

(defun accept-plist (p)
  (declare (tart ((plist keyword string)) -> string))
  (plist-get p :name))

;; Wrong order: string first, keyword second
(defun bad-alternation ()
  (accept-plist (cons "Alice" (cons :name nil))))
