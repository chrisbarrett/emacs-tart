;; Regression: subtracting a union from a union only removed exact whole-union matches
;; Fixed: iteration 30 (subtract_type now removes per-member in union subtrahend)
;; test: emacs-version 31.0

;; sequencep's else-branch subtracts (string | (list any) | (vector any))
;; from (string | int | (list any)), leaving int.
(defun sequencep-else-narrows (x)
  (declare (tart ((string | int | (list any))) -> int))
  (if (sequencep x)
      0
    (+ x 1)))

;; stringp subtraction from a 3-member union leaves the other two.
(defun stringp-else-leaves-union (x)
  (declare (tart ((string | int | symbol)) -> (int | symbol)))
  (if (stringp x)
      0
    x))
