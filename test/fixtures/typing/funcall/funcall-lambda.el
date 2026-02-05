;; Test: funcall with inline lambda
;; test: emacs-version 31.0

;; This should pass - lambda takes int, returns int, gets passed int
(funcall (lambda (x) (1+ x)) 5)
