;; Test: #'name looks up function in function namespace
;; test: emacs-version 31.0

(defun add-one (n)
  (1+ n))

;; #'add-one should have type (-> (Int) Int)
;; funcall should then accept an int argument
(funcall #'add-one 5)
