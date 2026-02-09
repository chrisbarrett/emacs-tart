;; Bounded quantification: rest-param union rejects out-of-bound types (Spec 87)
;; test: emacs-version 31.0
;;
;; When a function has (&rest (string | symbol)), passing int should fail
;; because int is not a subtype of (string | symbol).

;; Helper with rest-param union type
(defun rest-union-fn (x &rest args)
  (declare (tart (string &rest (string | symbol)) -> string))
  x)

;; Case: call with int arg → should fail (int not <: string | symbol)
(defun call-with-int ()
  (declare (tart () -> string))
  (rest-union-fn "fmt" 42))
