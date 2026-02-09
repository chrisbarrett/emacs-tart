;; Bounded quantification: multiple rest args (Spec 87)
;; test: emacs-version 31.0
;;
;; Calling a rest-param function with multiple arguments — each arg is
;; independently checked against the rest element union bound.

;; Function with rest-param union
(defun multi-rest (&rest args)
  (declare (tart (&rest (string | symbol)) -> string))
  "ok")

;; Two string args — both match string <: (string | symbol)
(defun two-strings ()
  (declare (tart () -> string))
  (multi-rest "a" "b"))

;; String and symbol — each matches independently
(defun str-and-sym ()
  (declare (tart () -> string))
  (multi-rest "hello" 'world))

;; Three mixed args — all pass
(defun three-mixed ()
  (declare (tart () -> string))
  (multi-rest "a" 'b "c"))

;; Single arg — pass
(defun single-arg ()
  (declare (tart () -> string))
  (multi-rest "only"))

;; No args — pass (empty rest)
(defun no-args ()
  (declare (tart () -> string))
  (multi-rest))
