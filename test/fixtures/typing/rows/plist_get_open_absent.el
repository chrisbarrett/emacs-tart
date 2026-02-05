;; Case 5 from Spec 11 R5: keyword absent from open row returns (α | nil)
;; The open row might contain the field in its unknown tail
;; test: emacs-version 31.0

(defun get-maybe-field (person)
  (declare (tart [r] ((plist {:name string & r})) -> (int | nil)))
  (plist-get person :age))
