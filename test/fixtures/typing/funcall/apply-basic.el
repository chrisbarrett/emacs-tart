;; Test: apply with rest-arg function

;; + has type (-> (&rest Int) Int), so apply should work with a list of ints
(apply #'+ '(1 2 3))
