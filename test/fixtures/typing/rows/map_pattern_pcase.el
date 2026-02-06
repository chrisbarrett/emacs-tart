;; R6: map pattern in regular pcase (not pcase-let)
;; Map patterns work in pcase clauses, constraining the scrutinee type.
;; test: emacs-version 31.0

(defun get-name-or-default (person)
  (declare (tart [r] ((map {name string & r}) -> string)))
  (pcase person
    ((map :name) name)
    (_ "unknown")))
