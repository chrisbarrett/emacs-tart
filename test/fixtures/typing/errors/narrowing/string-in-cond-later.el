;; Cumulative narrowing in cond: string-to-char used after stringp clause
;; In the second clause, x has been subtracted of string (from the first
;; clause's stringp test), so string-to-char should fail with int.
;; test: emacs-version 31.0

(defun cond-later-clause (x)
  (declare (tart ((string | int)) -> int))
  (cond
   ((stringp x) 0)
   (t (string-to-char x))))
