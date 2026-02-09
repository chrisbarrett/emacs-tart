;; declare-function, cl--defalias, defface, defgroup — make names known
;; test: emacs-version 31.0

(declare-function some-external-fn "some-file" (x y))

(defface my-face '((t :bold t))
  "A test face.")

(defgroup my-group nil
  "A test group.")

;; All names should be known as variables (no UNDEFINED VARIABLE)
(defun use-declared-fn ()
  some-external-fn)

(defun use-face ()
  my-face)

(defun use-group ()
  my-group)
