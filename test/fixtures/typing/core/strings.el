;; String primitives test fixture
;; Tests: concat string-equal string= string< string>

;; Passing cases - string construction
(defun string-concat ()
  (concat "hello" " " "world"))

(defun make-str ()
  (make-string 10 ?x))

;; Passing cases - string comparison
(defun strings-equal ()
  (string-equal "foo" "foo"))

(defun strings-eq ()
  (string= "foo" "foo"))

(defun strings-less ()
  (string< "a" "b"))

(defun strings-greater ()
  (string> "b" "a"))

;; Passing cases - string search
(defun find-in-string ()
  (string-search "world" "hello world"))

;; Passing cases - string bytes
(defun get-string-bytes ()
  (string-bytes "hello"))
