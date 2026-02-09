;; Bounded quantification: rest-param union produces upper bounds (Spec 87)
;; test: emacs-version 31.0
;;
;; When a function with (&rest (string | symbol)) is called, arguments in
;; rest positions are checked against the union bound. Subtypes pass.

;; Helper with rest-param union type
(defun rest-union-fn (x &rest args)
  (declare (tart (string &rest (string | symbol)) -> string))
  x)

;; Case 1: call with string rest args — pass (string <: string | symbol)
(defun call-with-strings ()
  (declare (tart () -> string))
  (rest-union-fn "hello" "world"))

;; Case 2: call with symbol rest args — pass (symbol <: string | symbol)
(defun call-with-symbols ()
  (declare (tart () -> string))
  (rest-union-fn "fmt" 'foo 'bar))

;; Case 3: call with mixed string and symbol — pass
(defun call-with-mixed ()
  (declare (tart () -> string))
  (rest-union-fn "fmt" "hello" 'world))

;; Case 4: no rest args at all — pass (zero args matches &rest)
(defun call-with-none ()
  (declare (tart () -> string))
  (rest-union-fn "fmt"))
