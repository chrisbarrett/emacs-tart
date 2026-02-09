;; Regression: car on a wide union type uses the any fallback clause
;; Fixed: Spec 81 — broadened car/cdr with (any) -> any fallback
;; test: emacs-version 31.0

;; car on a symbol triggers the any fallback clause with a note
(defun car-on-symbol (x)
  (declare (tart (symbol) -> any))
  (car x))
