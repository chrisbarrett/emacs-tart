;; eq with compatible types: same type, polymorphic, numeric subtyping
;; test: emacs-version 31.0

;; Same types
(defun eq-symbols (a b)
  (declare (tart (symbol symbol) -> bool))
  (eq a b))

;; Numeric subtyping: int and num are NOT disjoint
(defun eq-num-int (n i)
  (declare (tart (num int) -> bool))
  (eql n i))

;; nil with nil
(defun eq-nils ()
  (declare (tart () -> bool))
  (eq nil nil))
