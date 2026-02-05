;; Cases 1-2 from Spec 11 R5: literal keyword in closed row returns exact type
;; test: emacs-version 31.0

(defun get-name (person)
  (declare (tart ((plist {:name string :age int})) -> string))
  (plist-get person :name))
