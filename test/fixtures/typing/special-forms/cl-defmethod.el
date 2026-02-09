;; cl-defmethod with qualifier and specializer — expands to defun
;; test: emacs-version 31.0

(cl-defmethod my-method :around ((obj string) n)
  (+ (length obj) n))

(defun use-method ()
  (my-method "hello" 1))
