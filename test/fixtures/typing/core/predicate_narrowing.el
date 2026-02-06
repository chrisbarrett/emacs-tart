;; Predicate narrowing in if/then/else branches
;; Tests: stringp integerp narrowing in conditionals
;; test: emacs-version 31.0

;; R1: stringp narrows (string | int) to string in then-branch
(defun narrow-on-stringp (x)
  (declare (tart ((string | int)) -> int))
  (if (stringp x)
      (string-to-char x)
    (+ x 1)))

;; R1: integerp narrows (string | int) to int in then-branch
(defun narrow-on-integerp (x)
  (declare (tart ((string | int)) -> int))
  (if (integerp x)
      (+ x 1)
    (string-to-char x)))

;; R2: else-branch of stringp on 3-member union subtracts string
(defun stringp-else-branch (x)
  (declare (tart ((string | int | symbol)) -> int))
  (if (stringp x)
      (string-to-char x)
    0))

;; symbolp narrows (string | symbol) to symbol in then-branch
(defun narrow-on-symbolp (x)
  (declare (tart ((string | symbol)) -> string))
  (if (symbolp x)
      (symbol-name x)
    x))
