;; Exhaustiveness: pcase missing constructor case
;;
;; Note: Exhaustiveness warnings require ADT types defined in .tart files.
;; Without a companion .tart file defining an ADT, pcase patterns on
;; unknown types don't generate warnings.
;;
;; This fixture demonstrates a pcase that would be non-exhaustive if
;; the scrutinee were a known ADT type. Until fixture pairs (.el + .tart)
;; are supported, this passes silently.

(defun handle-number (x)
  "Handle specific numbers, but missing default case."
  (pcase x
    (1 "one")
    (2 "two")))
