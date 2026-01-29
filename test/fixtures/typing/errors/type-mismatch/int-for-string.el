;; Type mismatch: passing int where string expected
;; Error: concat expects strings but receives integer

(defun broken-concat ()
  "Concatenate should only accept strings."
  (concat "hello" 42))
