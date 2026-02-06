;; Row-typed hash-table: gethash with literal symbol key
;; Case 1 from Spec 11 R4: key in row → returns field_type directly
;; test: emacs-version 31.0

(defun get-person-name (person)
  (declare (tart [r] ((hash-table {name string & r})) -> string))
  (gethash 'name person))
