;; Case 5 from Spec 11 R4: key absent from open row returns (α | nil)
;; The open row might contain the field in its unknown tail
;; test: emacs-version 31.0

(defun get-maybe-field (person)
  (declare (tart [r] ((alist {name string & r})) -> (int | nil)))
  (alist-get 'age person))
