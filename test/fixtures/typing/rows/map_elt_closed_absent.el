;; Case 3 from Spec 11 R4: literal key absent from closed row returns nil
;; map-elt with a closed map row that does not contain the accessed key.
;; test: emacs-version 31.0

(defun get-missing (m)
  (declare (tart ((map {name string age int})) -> nil))
  (map-elt m 'email))
