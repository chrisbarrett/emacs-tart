;; eq with identity-safe types: symbol, keyword, int, t, nil
;; test: emacs-version 31.0

;; symbol is eq-safe
(defun eq-symbols (a b)
  (declare (tart (symbol symbol) -> bool))
  (eq a b))

;; keyword is eq-safe
(defun eq-keywords (a b)
  (declare (tart (keyword keyword) -> bool))
  (eq a b))

;; int is eq-safe
(defun eq-ints (a b)
  (declare (tart (int int) -> bool))
  (eq a b))

;; literal t is eq-safe
(defun eq-t ()
  (declare (tart () -> bool))
  (eq t t))

;; nil is eq-safe
(defun eq-nil ()
  (declare (tart () -> bool))
  (eq nil nil))
