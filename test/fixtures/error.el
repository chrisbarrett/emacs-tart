;; An Elisp file with a type error

(defun add (x y)
  (+ x y))

(add 1 "hello")
