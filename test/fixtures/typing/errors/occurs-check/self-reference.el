;; Regression: Self-referential type causes infinite type error
;; This creates a type variable that must equal a type containing itself

;; This creates a cyclic type constraint: the function type must be
;; (-> (a) (pair a (-> (a) ...))) which is infinite
(defun cycle (f)
  "Passes a function to cons with itself - creates circular type."
  (cons f f))
