;; Stored predicate result does NOT narrow (R12)
;; Inline-only restriction: binding predicate to a variable prevents narrowing
;; test: emacs-version 31.0

;; Storing stringp result in a variable: x should NOT be narrowed
(defun stored-predicate (x)
  (declare (tart ((string | int)) -> int))
  (let ((is-str (stringp x)))
    (if is-str
        (string-to-char x)
      (+ x 1))))
