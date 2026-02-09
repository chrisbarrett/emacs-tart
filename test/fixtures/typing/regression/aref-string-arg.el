;; Regression: aref/aset should accept both vectors and strings
;; Fixed: Spec 76 Iteration 4 (validation against Emacs lisp/)
;; test: emacs-version 31.0

;; aref on a vector should return the element type.
(defun get-vector-elem (v)
  (declare (tart ((vector int) int) -> int))
  (aref v 0))

;; aref on a string should return int (character code).
(defun get-string-char (s)
  (declare (tart (string int) -> int))
  (aref s 0))

;; aset on a string should accept int value and return int.
(defun set-string-char (s c)
  (declare (tart (string int int) -> int))
  (aset s 0 c))
