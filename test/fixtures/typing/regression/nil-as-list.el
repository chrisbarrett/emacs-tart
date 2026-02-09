;; Regression: nil should be accepted where (list a) is expected
;; Fixed: Spec 81 — nil <: (list a) subtyping rule
;; test: emacs-version 31.0

;; nil passed as a list argument to append (expects (list a))
(defun append-nil (xs)
  (declare (tart ((list int)) -> (list int)))
  (append xs nil))

;; nil returned where (list a) is the declared return type
(defun nil-as-empty-list ()
  (declare (tart () -> (list int)))
  nil)

;; nil in a conditional where both branches are (list a)
(defun maybe-nil-list (flag xs)
  (declare (tart (bool (list int)) -> (list int)))
  (if flag xs nil))
