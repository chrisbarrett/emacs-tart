;; defsubst expands to defun — function should be callable
;; test: emacs-version 31.0

(defsubst my-add (a b)
  (+ a b))

(defun use-defsubst ()
  (my-add 1 2))
