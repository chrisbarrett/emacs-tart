;; Type mismatch: passing string where int expected
;; Error: + expects numbers but receives string
;; test: emacs-version 31.0

(defun broken-add ()
  "Addition should only accept numbers."
  (+ 1 "two" 3))
