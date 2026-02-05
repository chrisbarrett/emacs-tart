;; Control flow primitives test fixture
;; Tests: funcall apply signal throw run-hooks
;; test: emacs-version 31.0

;; Passing cases - funcall and apply
(defun call-func ()
  (funcall #'+ 1 2 3))

(defun apply-func ()
  (apply #'+ '(1 2 3)))

(defun funcall-lambda ()
  (funcall (lambda (x) (+ x 1)) 5))

;; Passing cases - hooks
(defun run-a-hook ()
  (run-hooks 'after-init-hook))

(defun run-hook-args ()
  (run-hook-with-args 'some-hook 1 2 3))

;; Passing cases - command predicates
(defun check-commandp ()
  (commandp 'save-buffer))

;; Passing cases - error handling setup
;; Note: actual error/signal would abort, but type-check is ok
(defun make-error-args ()
  :string:
  (format "error: %s" "something went wrong"))

;; Passing cases - macro expansion
(defun expand-macro ()
  (macroexpand '(when t (message "hi"))))

;; Passing cases - backtrace info
(defun get-backtrace-frames ()
  (backtrace-frames))
