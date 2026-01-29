;; Unbound identifier: realistic typo in common function name
;; Error: Typo in function name should suggest the correct spelling

(defun typo-in-concat ()
  "Typo in common function name 'concat'."
  (cocnat "hello" "world"))
