;; Type mismatch: assigning int to function-typed variable
;; Error: callback expects function type (int) -> string but assigned int

(defvar my-callback nil)
(tart-declare my-callback ((int) -> string))

(defun broken-assignment ()
  "Try to assign an integer to a function-typed variable."
  (setq my-callback 42))
