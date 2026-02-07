;; Plist intrinsic type: subsumption and structural promotion
;; test: emacs-version 31.0

;; Subsumption: plist passes where list expected
(defun plist-to-list (p)
  (declare (tart ((plist keyword string)) -> (list (keyword | string))))
  p)

;; Cons chain with alternating k-v promotes to plist
(defun accept-plist (p)
  (declare (tart ((plist keyword string)) -> string))
  (plist-get p :name))

(defun cons-chain-promotes ()
  (declare (tart () -> string))
  (accept-plist (cons :name (cons "Alice" nil))))

;; Multi-pair cons chain promotes to plist
(defun cons-chain-multi ()
  (declare (tart () -> string))
  (accept-plist (cons :name (cons "Alice" (cons :age (cons "30" nil))))))

;; Row-typed plist-get still works with intrinsic form
(defun row-typed-get (p)
  (declare (tart [r] ((plist {:name string & r})) -> string))
  (plist-get p :name))
