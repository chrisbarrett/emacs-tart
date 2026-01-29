;; Unbound identifier: call to undefined function
;; Error: Calling a function that doesn't exist

(defun call-undefined ()
  "Try to call a function that doesn't exist."
  (undefined-function 1 2 3))
