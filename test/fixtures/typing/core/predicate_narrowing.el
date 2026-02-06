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

;; when narrows predicate in body (Spec 52 R1)
(defun when-stringp (x)
  (declare (tart ((string | int)) -> int))
  (when (stringp x)
    (string-to-char x)))

;; unless narrows via subtraction in body (Spec 52 R2)
(defun unless-stringp (x)
  (declare (tart ((string | int)) -> int))
  (unless (stringp x)
    (+ x 1)))

;; when with multi-expression body (implicit progn)
(defun when-progn-body (x)
  (declare (tart ((string | int)) -> int))
  (when (stringp x)
    (message "got a string")
    (string-to-char x)))

;; R3: Cumulative narrowing in cond - second clause sees string subtracted
(defun cond-cumulative (x)
  (declare (tart ((string | int | symbol)) -> int))
  (cond
   ((stringp x) (string-to-char x))
   ((symbolp x) (string-to-char (symbol-name x)))
   (t (+ x 1))))

;; R3: Cumulative narrowing - each clause narrows body correctly
(defun cond-narrowing-body (x)
  (declare (tart ((string | int)) -> int))
  (cond
   ((stringp x) (string-to-char x))
   ((integerp x) (+ x 1))))

;; R4: Predicate in and narrows for subsequent args
(defun and-stringp (x)
  (declare (tart ((string | int)) -> int))
  (when (and (stringp x) (> (string-to-char x) 0))
    42))

;; R4: Multiple predicates in and accumulate narrowing
(defun and-two-predicates (x)
  (declare (tart ((string | int | symbol)) -> int))
  (if (and (stringp x) (> (length x) 0))
      (string-to-char x)
    0))
