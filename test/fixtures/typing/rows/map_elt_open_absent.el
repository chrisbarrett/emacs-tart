;; Case 5 from Spec 11 R4: literal key absent from open row returns (α | nil)
;; map-elt with an open map row that does not contain the accessed key.
;; test: emacs-version 31.0

(defun get-maybe-field (m)
  (declare (tart [r] ((map {name string & r})) -> (int | nil)))
  (map-elt m 'age))
