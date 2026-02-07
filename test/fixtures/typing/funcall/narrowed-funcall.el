;; Test: R12/R13 - occurrence typing narrows function types for funcall
;; test: emacs-version 31.0

;; R13: functionp narrows to function type, enabling funcall
(defun call-if-function (f x)
  (declare (tart (any any) -> any))
  (if (functionp f)
      (funcall f x)
    nil))

;; R12: stringp narrows type, narrowed value used in funcall context
(defun call-on-string (x)
  (declare (tart ((string | int)) -> int))
  (if (stringp x)
      (funcall #'string-to-char x)
    0))
