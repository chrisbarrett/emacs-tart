;; Row-typed hash-table with closed row: absent key returns nil
;; Case 3 from Spec 11 R4: key absent from closed row → nil
;; test: emacs-version 31.0

(defun get-missing (tbl)
  (declare (tart ((hash-table {name string age int})) -> nil))
  (gethash 'email tbl))
