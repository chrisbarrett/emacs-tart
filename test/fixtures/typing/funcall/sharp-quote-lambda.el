;; Test: #'(lambda ...) returns the lambda's function type

;; This should type-check: #'(lambda (x) (1+ x)) has type (-> (Int) Int)
(funcall #'(lambda (x) (1+ x)) 5)
