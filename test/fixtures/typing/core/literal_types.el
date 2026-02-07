;; Literal types with deferred widening (Spec 11 R9)
;; Tests: literal type preservation in let, widening to declared types,
;; widening in arithmetic, literal type mismatch across base types
;; test: emacs-version 31.0

;; Integer literal widens to declared int return
(defun int-lit-widens ()
  (declare (tart () -> int))
  42)

;; Float literal widens to declared float return
(defun float-lit-widens ()
  (declare (tart () -> float))
  1.0)

;; String literal widens to declared string return
(defun string-lit-widens ()
  (declare (tart () -> string))
  "hello")

;; Keyword literal widens to declared keyword return
(defun keyword-lit-widens ()
  (declare (tart () -> keyword))
  :foo)

;; Quoted symbol widens to declared symbol return
(defun symbol-lit-widens ()
  (declare (tart () -> symbol))
  'bar)

;; Let-bound literal widens when used in arithmetic
(defun lit-in-let-arithmetic ()
  (declare (tart () -> int))
  (let ((x 42))
    (+ x 1)))

;; Literal list elements widen to list element type
(defun lit-in-list ()
  (declare (tart () -> (list int)))
  (list 1 2 3))

;; String concat with literals widens properly
(defun string-concat-lits ()
  (declare (tart () -> string))
  (concat "hello" " " "world"))
