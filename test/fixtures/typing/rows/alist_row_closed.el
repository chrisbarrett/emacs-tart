;; Row-typed alist: accessing absent key from closed row
;; test: emacs-version 31.0

(defun get-missing-field (person)
  (declare (tart ((alist {name string})) -> (int | nil)))
  (alist-get 'age person))
