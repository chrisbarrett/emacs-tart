;; Cases 1-2 from Spec 11 R4: literal key in row returns exact type
;; test: emacs-version 31.0

(defun get-name (person)
  (declare (tart ((alist {name string age int})) -> string))
  (alist-get 'name person))
