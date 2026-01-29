;; Kind Error: Using value where type expected
;;
;; Note: In inline type annotations, literals like numbers are not
;; valid type syntax. The type checker recognizes declare syntax
;; specially, so invalid type forms produce parse errors.
;;
;; This fixture demonstrates attempting to use a literal in a type
;; annotation position where a type name is expected.

(defun example (x)
  (declare (tart (42) -> Int))
  (+ x 1))
