;; Row-typed alist: alist-get with literal symbol key
;; test: emacs-version 31.0

(defun get-person-name (person)
  (declare (tart [r] ((alist {name string & r})) -> (string | nil)))
  (alist-get 'name person))
