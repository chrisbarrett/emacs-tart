;; Regression: plist values were not accepted where list was expected
;; Fixed: iteration 13 (plist-to-list one-way subsumption in unify.ml)
;; test: emacs-version 31.0

;; A plist should widen to list of its key|value union.
(defun plist-as-list (p)
  (declare (tart ((plist keyword string)) -> (list (keyword | string))))
  p)

;; Cons chain structural promotion to plist (iteration 14).
(defun accept-plist (p)
  (declare (tart ((plist keyword string)) -> string))
  (plist-get p :name))

(defun cons-chain-to-plist ()
  (declare (tart () -> string))
  (accept-plist (cons :name (cons "hello" nil))))
