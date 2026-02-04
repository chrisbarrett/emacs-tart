;; Type mismatch: passing int where string expected
;; Error: make-symbol expects a string but receives integer

(defun broken-make-symbol ()
  "make-symbol should only accept strings."
  (make-symbol 42))
