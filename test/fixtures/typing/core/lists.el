;; List primitives test fixture
;; Tests: car cdr cons list nth mapcar length append reverse
;; test: emacs-version 31.0

;; Passing cases - list construction
(defun make-list-literal ()
  (list 1 2 3))

(defun make-cons-cell ()
  (cons 1 '(2 3)))

;; Passing cases - list access
(defun get-car ()
  (car '(1 2 3)))

(defun get-cdr ()
  (cdr '(1 2 3)))

(defun get-nth ()
  (nth 1 '(a b c)))

(defun get-last ()
  (last '(1 2 3)))

;; Passing cases - sequence operations
(defun get-length ()
  (length '(1 2 3)))

(defun list-append ()
  (append '(1 2) '(3 4)))

(defun list-reverse ()
  (reverse '(1 2 3)))

;; Passing cases - higher-order functions
(defun map-add1 ()
  (mapcar #'1+ '(1 2 3)))

(defun map-with-lambda ()
  (mapcar (lambda (x) (+ x 1)) '(1 2 3)))

;; Passing cases - type preservation
(defun map-preserves-type ()
  :int:
  (car (mapcar #'1+ '(1 2 3))))
