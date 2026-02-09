;; Bounded quantification: non-rest union uses equality, not bounds (Spec 87)
;; test: emacs-version 31.0
;;
;; Only rest-param unions produce upper bounds. Fixed params with union types
;; use normal equality constraints. This fixture verifies that fixed-param
;; union behavior is unchanged by bounded quantification.

;; Fixed param accepts (int | string) union
(defun fixed-union-fn (x)
  (declare (tart ((int | string)) -> string))
  "ok")

;; Call with int — pass (int <: int | string)
(defun call-fixed-int ()
  (declare (tart () -> string))
  (fixed-union-fn 42))

;; Call with string — pass (string <: int | string)
(defun call-fixed-string ()
  (declare (tart () -> string))
  (fixed-union-fn "hello"))
