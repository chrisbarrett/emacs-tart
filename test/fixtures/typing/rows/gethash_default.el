;; Case 4 from Spec 11 R4: literal key absent from closed row with default
;; gethash returns the default value type when the key is not in the closed row.
;; test: emacs-version 31.0

(defun get-missing-with-default (tbl)
  (declare (tart ((hash-table {name string age int})) -> int))
  (gethash 'email tbl 0))
