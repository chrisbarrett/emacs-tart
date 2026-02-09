;; Regression: car/cdr should accept non-list types via any fallback clause
;; Fixed: Spec 81 — broadened car/cdr signatures
;; test: emacs-version 31.0

;; car on a list — uses the (list a) clause
(defun car-on-list (xs)
  (declare (tart ((list int)) -> (int | nil)))
  (car xs))

;; cdr on a list — uses the (list a) clause
(defun cdr-on-list (xs)
  (declare (tart ((list int)) -> (list int)))
  (cdr xs))

;; car on a non-list type — uses the (any) fallback clause with note
(defun car-on-non-list (x)
  (declare (tart ((cons int string)) -> any))
  (car x))
