;; Predicate primitives test fixture
;; Tests: null listp stringp numberp integerp symbolp atom

;; Passing cases - type predicates
(defun check-null ()
  (null '()))

(defun check-atom ()
  (atom 'foo))

(defun check-listp ()
  (listp '(1 2 3)))

(defun check-consp ()
  (consp '(1 . 2)))

(defun check-symbolp ()
  (symbolp 'foo))

(defun check-stringp ()
  (stringp "hello"))

(defun check-numberp ()
  (numberp 42))

(defun check-integerp ()
  (integerp 42))

(defun check-floatp ()
  (floatp 3.14))

(defun check-functionp ()
  (functionp #'car))

;; Passing cases - equality predicates
(defun check-eq ()
  (eq 'foo 'foo))

;; Note: eql is not in builtins, use eq instead
(defun check-eql ()
  (eq 1 1))

(defun check-equal ()
  (equal '(1 2) '(1 2)))
