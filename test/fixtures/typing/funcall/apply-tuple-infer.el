;; Test: quoted list infers as tuple in apply context (Spec 34 R10)
;; test: emacs-version 31.0

;; Quoted list '(1 2 3) infers as (tuple int int int)
;; + : (-> (&rest int) int), tuple widens to (list int) via R9
(apply #'+ '(1 2 3))

;; Mixed fixed and list args: fixed arg 10 is int, '(20 30) → (tuple int int)
(apply #'+ 10 '(20 30))

;; Type error: quoted list '("hello" 1) gives (tuple string int)
;; + expects (&rest int), so "hello" is a type mismatch
(apply #'+ '("hello" 1))
