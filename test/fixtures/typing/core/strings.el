;; String primitives test fixture
;; Tests: concat substring string-length upcase downcase

;; Passing cases - string construction
(defun string-concat ()
  (concat "hello" " " "world"))

;; TODO: substring with optional args has a unification bug (BUG-002)
;; When providing optional args, the type checker incorrectly infers
;; partial application instead of a complete call. Skipping this test.
;; (defun get-substring ()
;;   (substring "hello" 0 3))

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
