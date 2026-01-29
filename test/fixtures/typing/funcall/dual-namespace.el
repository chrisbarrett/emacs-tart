;; Test: same name can exist in variable and function namespaces

;; Define a function named 'foo'
(defun foo (x)
  (1+ x))

;; Define a variable also named 'foo' with a different type
;; In a let binding, the variable is in a separate namespace from the function
(let ((foo "a string"))
  ;; Variable 'foo' is a string
  (concat foo " bar")
  ;; #'foo refers to the function, not the variable
  (funcall #'foo 42))
