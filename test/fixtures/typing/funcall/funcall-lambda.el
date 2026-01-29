;; Test: funcall with inline lambda

;; This should pass - lambda takes int, returns int, gets passed int
(funcall (lambda (x) (1+ x)) 5)
