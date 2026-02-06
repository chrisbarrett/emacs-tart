;; R6: map pattern extracts typed fields from row-typed value
;; pcase-let with (map :key) pattern constrains expression to map type
;; and binds extracted fields with their row field types.
;; test: emacs-version 31.0

(defun get-name (person)
  (declare (tart [r] ((map {name string & r}) -> string)))
  (pcase-let (((map :name) person))
    name))
