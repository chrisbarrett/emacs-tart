;; Multi-clause defun signatures test fixture
;; Tests: single-clause compat, predicate narrowing via clause derivation
;; test: emacs-version 31.0

;; R2: Single-clause defun backward compatibility
(defun single-clause-add (x y)
  (declare (tart (int int) -> int))
  (+ x y))

;; R5+R6: sequencep narrows via multi-type predicate (clause-derived)
;; sequencep clauses: (((list any)) -> t) (((vector any)) -> t) ((string) -> t) ((_) -> nil)
;; Then-branch keeps only sequence-compatible members of the union
(defun sequencep-narrows-to-string (x)
  (declare (tart ((string | int)) -> int))
  (when (sequencep x)
    (string-to-char x)))

;; R5: numberp narrows (clause-derived from ((num) -> t) ((_) -> nil))
;; Verifies multi-clause derived predicates interop with cond narrowing
(defun numberp-and-stringp-cond (x)
  (declare (tart ((string | int | symbol)) -> int))
  (cond
   ((stringp x) (string-to-char x))
   ((integerp x) (+ x 1))
   (t 0)))

;; R4: Multi-clause type computation produces correct overall type
;; stringp accepts any -> bool, narrowing works in conditional
(defun stringp-multi-clause-narrows (x)
  (declare (tart ((string | int)) -> string))
  (if (stringp x)
      x
    "not a string"))

;; R8: Polymorphic single-clause car works on cons list
(defun polymorphic-car-on-list (xs)
  (declare (tart ((list int)) -> (int | nil)))
  (car xs))

;; R5+R6: clause-derived predicates work in cond with cumulative narrowing
;; After stringp clause, remaining is (int | symbol). After symbolp, only int.
(defun clause-predicates-in-cond (x)
  (declare (tart ((string | int | symbol)) -> int))
  (cond
   ((stringp x) (string-to-char x))
   ((symbolp x) (string-to-char (symbol-name x)))
   (t (+ x 1))))
