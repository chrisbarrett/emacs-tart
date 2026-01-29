;; Regression: Mutually dependent types create infinite type
;; When two expressions constrain each other's types cyclically

;; Here we create a value and try to make it equal to a list containing itself
;; This creates the constraint: 'a = (List 'a) which fails occurs check
(defun make-cycle (x)
  "Creates a list where the element is the list itself - type error."
  ;; The lambda's argument should be the same type as what cons returns
  ;; But cons returns (List 'a), so we get: 'a = (List 'a)
  ((lambda (y) (cons y y)) x))
