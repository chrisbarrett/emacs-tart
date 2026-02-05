;; Row-typed alist: alist-get with literal symbol key in open row
;; Case 1 from Spec 11 R4: key in row → returns field_type directly
;; test: emacs-version 31.0

(defun get-person-name (person)
  (declare (tart [r] ((alist {name string & r})) -> string))
  (alist-get 'name person))
