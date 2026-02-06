;; All map type forms (R13): homogeneous and record alist forms
;; Verifies homogeneous (alist k v) and record (alist {fields}) coexist.
;; test: emacs-version 31.0

;; Form 1: Record-style alist (open row) — literal key access
(defun get-name (person)
  (declare (tart [r] ((alist {name string & r})) -> string))
  (alist-get 'name person))

;; Form 2: Record-style alist (closed row) — literal key access
(defun get-age (person)
  (declare (tart ((alist {name string age int})) -> int))
  (alist-get 'age person))

;; Form 3: Inferred row — no annotation needed
(defun get-title (item)
  (1+ (alist-get 'count item)))
