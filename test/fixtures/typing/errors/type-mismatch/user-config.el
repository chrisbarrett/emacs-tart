;; Realistic user scenario: wrong type in defvar
;;
;; This fixture represents a common mistake in user configuration:
;; setting a variable to the wrong type and then using it incorrectly.

(defvar my-indent-size "4")   ;; String instead of number

(defun indent-code ()
  "Indent the current line by configured amount."
  ;; User intended a number but provided a string
  (+ 1 my-indent-size))
