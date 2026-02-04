;; Regression: Mutually dependent types create infinite type
;; When two functions constrain each other's types cyclically

;; Here we create two functions that pass each other as arguments
;; This creates a cyclic type constraint
(defun mutual-omega ()
  "Creates mutual recursion in types - omega with two functions."
  (let ((f (lambda (x) (funcall x x)))
        (g (lambda (y) (funcall y y))))
    (funcall f g)))
