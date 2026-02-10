;; Tuple element access via nth intrinsic (Spec 91)
;; Tests: nth returns precise element types for tuples with literal indices
;; test: emacs-version 31.0

;; Literal index on heterogeneous tuple: (nth 0 ...) → first element type
(defun nth-tuple-first ()
  :symbol:
  (nth 0 (list 'setq 42)))

;; Literal index on heterogeneous tuple: (nth 1 ...) → second element type
(defun nth-tuple-second ()
  :int:
  (nth 1 (list 'setq 42)))

;; Out-of-bounds literal index on tuple → nil
(defun nth-tuple-oob ()
  :nil:
  (nth 5 (list 'setq 42)))

;; Negative literal index on tuple → nil
(defun nth-tuple-neg ()
  :nil:
  (nth -1 (list 'setq 42)))

;; Non-literal index on tuple → union of all elements plus nil
(defun nth-tuple-dynamic (n)
  (declare (tart (int) -> (symbol | int | nil)))
  (nth n (list 'setq 42)))

;; Regular homogeneous list: falls back to signature-based typing
(defun nth-list-fallback ()
  (declare (tart () -> (int | nil)))
  (nth 0 (list 1 2 3)))

;; Tuple with three elements: access middle
(defun nth-tuple-middle ()
  :string:
  (nth 1 (list 42 "hello" t)))

;; elt intrinsic on tuple: literal index → precise type
(defun elt-tuple-first ()
  :symbol:
  (elt (list 'setq 42) 0))

;; elt with non-literal index on tuple → union of elements plus nil
(defun elt-tuple-dynamic (n)
  (declare (tart (int) -> (symbol | int | nil)))
  (elt (list 'setq 42) n))

;; elt on regular list: falls back to signature-based typing
(defun elt-list-fallback ()
  :int:
  (elt (list 1 2 3) 0))
