;; String primitives test fixture
;; Tests: concat substring string-length upcase downcase

;; Passing cases - string construction
(defun string-concat ()
  (concat "hello" " " "world"))

;; Passing cases - substring (only two args - third is optional)
(defun get-substring ()
  (substring "hello" 0))

;; Passing cases - string length
(defun get-length ()
  (string-length "hello"))

;; Passing cases - case conversion
(defun to-upper ()
  (upcase "hello"))

(defun to-lower ()
  (downcase "HELLO"))

;; Passing cases - format
(defun format-str ()
  (format "Hello %s" "world"))

;; Passing cases - conversion
(defun to-number ()
  (string-to-number "42"))

(defun to-string ()
  (number-to-string 42))
