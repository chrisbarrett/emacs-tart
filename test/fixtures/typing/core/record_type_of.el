;; Record type-of test fixture
;; Tests: type-of multi-clause returns symbol for records and non-records
;; test: emacs-version 31.0

;; type-of on a record returns the tag type (symbol, since literal widens)
(defun type-of-record ()
  (declare (tart () -> symbol))
  (type-of (record 'foo 1 2)))

;; type-of on a non-record returns symbol
(defun type-of-non-record ()
  (declare (tart () -> symbol))
  (type-of 42))

;; type-of on a record from make-record returns symbol
(defun type-of-make-record ()
  (declare (tart () -> symbol))
  (type-of (make-record 'bar 3 nil)))
