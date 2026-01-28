;; Arithmetic primitives test fixture
;; Tests: + - * / % 1+ 1- abs max min

;; Passing cases - basic arithmetic with integers
(defun add-numbers ()
  (+ 1 2 3))

(defun subtract-numbers ()
  (- 10 5))

(defun multiply-numbers ()
  (* 2 3 4))

(defun divide-numbers ()
  (/ 10 2))

(defun modulo-int ()
  (% 10 3))

(defun increment ()
  (1+ 5))

(defun decrement ()
  (1- 5))

(defun absolute-value ()
  (abs -5))

(defun find-max ()
  (max 1 2 3))

(defun find-min ()
  (min 1 2 3))

;; Passing cases - comparisons
(defun compare-less ()
  (< 1 2))

(defun compare-greater ()
  (> 2 1))

(defun compare-equal ()
  (= 1 1))

;; Passing cases - logical operations
(defun bitwise-and ()
  (logand 7 3))

(defun bitwise-or ()
  (logior 4 2))

(defun bitwise-xor ()
  (logxor 5 3))

(defun bitwise-not ()
  (lognot 0))
