;; Type mismatch: passing string where int expected
;; Error: + expects numbers but receives string

(defun broken-add ()
  "Addition should only accept numbers."
  (+ 1 "two" 3))
