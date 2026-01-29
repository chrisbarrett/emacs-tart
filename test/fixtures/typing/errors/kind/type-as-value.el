;; Kind Error: Using type constructor as value
;;
;; Note: In tart, type names like Int, String, List are not bound
;; as values. Using them in value position produces an unbound variable
;; error rather than a kind error. True kind errors occur in .tart files
;; when type variables are used inconsistently (e.g., at different arities).
;;
;; This fixture demonstrates the user-facing behavior when someone
;; mistakenly tries to use a type name as a value.

(defun example ()
  (+ Int 1))
