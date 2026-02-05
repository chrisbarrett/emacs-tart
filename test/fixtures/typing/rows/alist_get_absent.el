;; Case 3 from Spec 11 R4: literal key absent from closed row returns nil
;; test: emacs-version 31.0

(defun get-missing (person)
  (declare (tart ((alist {name string})) -> nil))
  (alist-get 'age person))
