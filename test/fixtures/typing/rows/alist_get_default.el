;; Case 4 from Spec 11 R4: literal key absent from closed row with default
;; test: emacs-version 31.0

(defun get-missing-with-default (person)
  (declare (tart ((alist {name string})) -> int))
  (alist-get 'age person 0))
