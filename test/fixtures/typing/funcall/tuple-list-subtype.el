;; Test: tuple-to-list subtyping (Spec 34 R9)
;; test: emacs-version 31.0

;; Declare a function that returns a tuple of ints
(defvar get-pair nil)
(tart-declare get-pair (() -> (tuple int int)))

;; Declare a function that accepts a list of ints
(defvar sum-list nil)
(tart-declare sum-list (((list int)) -> int))

;; Tuple should widen to list: (tuple int int) <: (list int)
(sum-list (get-pair))

;; Declare a function that returns a mixed tuple
(defvar get-mixed nil)
(tart-declare get-mixed (() -> (tuple string int)))

;; Mixed tuple should NOT widen to (list int) — string /= int
(sum-list (get-mixed))
