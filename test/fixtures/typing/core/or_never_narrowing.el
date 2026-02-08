;; Or-expression narrowing via never-exit pattern (Spec 52 R5)
;; Tests: (or (pred x) (error ...)) narrows x for subsequent code
;; test: emacs-version 31.0

;; R5: stringp + error narrows x to string in subsequent code
(defun or-stringp-error (x)
  (declare (tart ((string | int)) -> int))
  (or (stringp x) (error "Expected string"))
  (string-to-char x))

;; R5: integerp + error narrows x to int in subsequent code
(defun or-integerp-error (x)
  (declare (tart ((string | int)) -> int))
  (or (integerp x) (error "Expected int"))
  (+ x 1))

;; R5: signal also returns never
(defun or-pred-signal (x)
  (declare (tart ((string | int)) -> int))
  (or (stringp x) (signal 'wrong-type-argument '("stringp" x)))
  (string-to-char x))

;; R5: throw also returns never
(defun or-pred-throw (x)
  (declare (tart ((string | int)) -> int))
  (or (stringp x) (throw 'done nil))
  (string-to-char x))

;; R5: multiple predicates on same variable merge to union narrowing
(defun or-same-var-multi-pred (x)
  (declare (tart ((string | int | symbol)) -> int))
  (or (stringp x) (integerp x) (error "Expected string or int"))
  (if (stringp x)
      (string-to-char x)
    (+ x 1)))
