;; Union intersection for predicate narrowing
;; Tests: multi-type predicates with else-branch subtraction
;; test: emacs-version 31.0

;; Else-branch: (string | int | (list any)) subtracting sequencep's
;; union (string | (list any) | (vector any)) leaves only int
(defun sequencep-else-branch (x)
  (declare (tart ((string | int | (list any))) -> int))
  (if (sequencep x)
      0
    (+ x 1)))

;; Both branches: full narrowing + subtraction with stringp
;; Then: (string | int) narrowed by stringp -> string
;; Else: (string | int) - string -> int
(defun stringp-both-branches (x)
  (declare (tart ((string | int)) -> int))
  (if (stringp x)
      (string-to-char x)
    (+ x 1)))

;; Else-branch with symbolp: (string | int | symbol) - symbol -> (string | int)
(defun symbolp-else-union-subtract (x)
  (declare (tart ((string | int | symbol)) -> int))
  (if (symbolp x)
      0
    (cond
     ((stringp x) (string-to-char x))
     (t (+ x 1)))))
