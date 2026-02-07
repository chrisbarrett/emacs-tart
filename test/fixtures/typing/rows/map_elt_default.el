;; Case 4 from Spec 11 R4: literal key absent from closed row with default
;; map-elt returns the default value type when the key is not in the closed row.
;; test: emacs-version 31.0

(defun get-missing-with-default (m)
  (declare (tart ((map {name string age int})) -> int))
  (map-elt m 'email 0))
