;; Type mismatch: passing list where atom (symbol) expected
;; Error: symbol-name expects a symbol but receives a list

(defun broken-symbol-name ()
  "Get the name of a symbol, but pass a list instead."
  (symbol-name '(not a symbol)))
