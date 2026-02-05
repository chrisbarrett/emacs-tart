;; Realistic user scenario: function call with wrong arity
;; test: emacs-version 31.0
;;
;; This fixture represents a common mistake when calling functions -
;; providing the wrong number of arguments.

;; nth expects exactly 2 arguments: index and list
;; User forgot to provide the list argument
(defun get-first (xs)
  "Get the first element of a list."
  (nth 0))
