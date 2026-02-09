;; Heterogeneous list inference (Spec 84)
;; Tests: list intrinsic infers TTuple for heterogeneous args
;; test: emacs-version 31.0

;; Homogeneous: all ints → (list int)
(defun homo-int ()
  :int:
  (car (list 1 2 3)))

;; Homogeneous: all strings → (list string)
(defun homo-string ()
  :string:
  (car (list "a" "b" "c")))

;; Single element → (list t)
(defun single-elem ()
  :int:
  (car (list 42)))

;; Empty list → nil
(defun empty-list ()
  :nil:
  (list))

;; Heterogeneous: symbol + int → TTuple, passes (list any) via subtyping
(defun hetero-to-list-any ()
  (declare (tart () -> (list any)))
  (list 'setq 42))

;; Code-as-data pattern: TTuple → (list any)
(defun code-as-data (place value)
  (declare (tart (symbol int) -> (list any)))
  (list 'setq place value))

;; Nested heterogeneous lists
(defun nested-hetero ()
  (declare (tart () -> (list any)))
  (list 'progn
        (list 'setq 'x 1)
        (list 'message "done")))

;; Mixed int/float → TTuple (int and float don't directly unify)
;; but passes (list num) via TTuple <: TList subtyping
(defun mixed-numeric ()
  (declare (tart () -> (list num)))
  (list 1 2.0 3))

;; Homogeneous variables
(defun homo-vars (a b c)
  (declare (tart (int int int) -> (list int)))
  (list a b c))

;; Heterogeneous variables
(defun hetero-vars (a b)
  (declare (tart (string int) -> (list any)))
  (list a b))
