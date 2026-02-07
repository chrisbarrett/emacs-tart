;; Unified row accessor decision table (Spec 56 Iteration 2)
;; Exercises all 7 cases from Spec 11 R4 via the unified infer_row_accessor.
;; test: emacs-version 31.0

;; Case 1: literal key present in open row → exact field type
(defun case1-key-in-open-row (person)
  (declare (tart [r] ((plist {:name string & r})) -> string))
  (plist-get person :name))

;; Case 2: literal key present in closed row → exact field type
(defun case2-key-in-closed-row (person)
  (declare (tart ((plist {:name string :age int})) -> string))
  (plist-get person :name))

;; Case 3: literal key absent from closed row, no default → nil
(defun case3-absent-closed (person)
  (declare (tart ((plist {:name string})) -> nil))
  (plist-get person :age))

;; Case 5: literal key absent from open row → (α | nil)
(defun case5-absent-open (person)
  (declare (tart [r] ((plist {:name string & r})) -> (int | nil)))
  (plist-get person :age))

;; R8: container type unknown → row inferred from usage
(defun r8-infer-from-usage (person)
  (1+ (plist-get person :age)))

;; Case 1 for alist-get: literal key in row → exact type
(defun alist-case1 (person)
  (declare (tart ((alist {name string age int})) -> int))
  (alist-get 'age person))

;; Case 4 for alist-get: absent from closed row with default → default type
(defun alist-case4 (person)
  (declare (tart ((alist {name string})) -> int))
  (alist-get 'age person 0))

;; Case 1 for gethash: literal key in row → exact type
(defun gethash-case1 (tbl)
  (declare (tart ((hash-table {name string age int})) -> int))
  (gethash 'age tbl))

;; Case 4 for gethash: absent from closed row with default → default type
(defun gethash-case4 (tbl)
  (declare (tart ((hash-table {name string})) -> int))
  (gethash 'email tbl 0))

;; Case 1 for map-elt: literal key in row → exact type
(defun map-elt-case1 (m)
  (declare (tart [r] ((map {id int & r})) -> int))
  (map-elt m 'id))
