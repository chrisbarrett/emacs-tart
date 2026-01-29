;; Kind Error: Type constructor with wrong number of arguments
;;
;; Note: The type checker currently does not check arity of type
;; constructors in inline annotations. This fixture shows a case
;; where Option is given two type arguments instead of one.
;;
;; In the future, this should produce a kind error indicating
;; that Option expects 1 argument but received 2.

(defun example (x)
  (declare (tart ((Option Int String)) -> Int))
  (length x))
