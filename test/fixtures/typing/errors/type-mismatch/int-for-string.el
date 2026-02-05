;; Type mismatch: passing int where string expected
;; Error: make-symbol expects a string but receives integer
;; test: emacs-version 31.0

(defun broken-make-symbol ()
  "make-symbol should only accept strings."
  (make-symbol 42))
