;; Case 5 from Spec 11 R4: literal key absent from open row returns (α | nil)
;; The open row might contain the field in its unknown tail.
;; test: emacs-version 31.0

(defun get-maybe-field (tbl)
  (declare (tart [r] ((hash-table {name string & r})) -> (int | nil)))
  (gethash 'age tbl))
