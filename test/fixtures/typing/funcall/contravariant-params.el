;; Test: contravariant function parameter subtyping (Spec 90)
;; test: emacs-version 31.0
;;
;; A function accepting wider parameter types is substitutable for one
;; accepting narrower types: (A -> R) <: (B -> R) when B <: A.

;; Case 1: callback with wider parameter type (any > string)
(defun process-item (handler)
  (declare (tart ((string -> nil)) -> nil))
  (funcall handler "hello"))

(defun handler-any (x)
  (declare (tart (any) -> nil))
  (message "got: %s" x))

(process-item #'handler-any)

;; Case 2: callback with narrower return type (string < any)
(defun call-producer (f)
  (declare (tart ((() -> any)) -> any))
  (funcall f))

(defun make-greeting ()
  (declare (tart () -> string))
  "hello")

(call-producer #'make-greeting)

;; Case 3: nullable parameter widens non-nullable
;; (string|nil -> nil) <: (string -> nil) because string <: string|nil
(defun takes-string-callback (f)
  (declare (tart ((string -> nil)) -> nil))
  (funcall f "hi"))

(defun nullable-handler (x)
  (declare (tart ((string | nil)) -> nil))
  (message "got: %s" x))

(takes-string-callback #'nullable-handler)

;; Case 4: numeric subtyping in parameter position
;; (num -> string) <: (int -> string) because int <: num
(defun int-formatter (f)
  (declare (tart ((int -> string)) -> string))
  (funcall f 42))

(defun format-num (n)
  (declare (tart (num) -> string))
  (number-to-string n))

(int-formatter #'format-num)
