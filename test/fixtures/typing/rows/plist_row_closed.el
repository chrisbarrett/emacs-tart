;; Row-typed plist: absent key from closed row returns nil
;; Case 3 from Spec 11 R5: literal key absent from closed row → nil
;; test: emacs-version 31.0

(defun get-missing (person)
  (declare (tart ((plist {:name string})) -> nil))
  (plist-get person :age))
