;; Row-typed plist: plist-get with literal keyword key in open row
;; Case 1 from Spec 11 R5: key in row → returns field_type directly
;; test: emacs-version 31.0

(defun get-person-name (person)
  (declare (tart [r] ((plist {:name string & r})) -> string))
  (plist-get person :name))
