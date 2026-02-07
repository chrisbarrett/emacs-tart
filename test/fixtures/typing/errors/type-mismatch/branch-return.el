;; Function body type incompatible with declared return type
;; Error: body returns string but function declares int return
;; test: emacs-version 31.0

(defun greeting ()
  (declare (tart () -> int))
  "hello")
