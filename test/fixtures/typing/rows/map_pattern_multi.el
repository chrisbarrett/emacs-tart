;; R6: map pattern with multiple fields and explicit variable binding
;; (:key var) syntax binds a named variable to the field's value.
;; test: emacs-version 31.0

(defun format-person (person)
  (declare (tart [r] ((map {name string age int & r}) -> string)))
  (pcase-let (((map :name (:age a)) person))
    (tart string (format "%s is %d" name a))))
