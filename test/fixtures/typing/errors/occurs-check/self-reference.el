;; Regression: Self-referential type causes infinite type error
;; This creates a type variable that must equal a type containing itself

;; The omega combinator: f must be a function that accepts itself as argument
;; This requires f : (-> (f) a) which creates an infinite type
(defun omega (f)
  "Applies f to itself - creates circular type."
  (funcall f f))
