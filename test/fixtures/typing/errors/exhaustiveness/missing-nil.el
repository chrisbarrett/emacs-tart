;; Exhaustiveness: list match missing nil case
;;
;; Note: Exhaustiveness warnings require ADT types defined in .tart files.
;; Without a companion .tart file, list patterns don't generate warnings
;; for missing nil cases.
;;
;; This fixture demonstrates a pcase on a list that only handles the
;; cons case, not the nil case (using explicit pattern match).

(defun list-length (xs)
  "Count list elements, missing empty list case."
  (pcase xs
    ((pred consp) 1)))
