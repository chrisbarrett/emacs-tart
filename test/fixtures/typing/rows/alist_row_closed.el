;; Row-typed alist: accessing absent key from closed row
;; Case 3 from Spec 11 R4: key absent from closed row → returns nil
;; test: emacs-version 31.0

(defun get-missing-field (person)
  (declare (tart ((alist {name string})) -> nil))
  (alist-get 'age person))
