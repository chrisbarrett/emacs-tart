;; condition-case return typing (Spec 85)
;; Tests: condition-case infers union of body and handler types
;; test: emacs-version 31.0

;; Body and handler have same type → returns that type
(defun cc-same-type ()
  :string:
  (condition-case nil
      "ok"
    (error "fallback")))

;; Body string, handler int → union type
(defun cc-union-type (input)
  (declare (tart (string) -> (string | int)))
  (condition-case nil
      (concat input " ok")
    (error 42)))

;; Body with never handler (error signals never return)
(defun cc-never-handler ()
  :string:
  (condition-case nil
      "ok"
    (error (error "re-raise"))))

;; condition-case with variable binding
(defun cc-with-var ()
  :string:
  (condition-case err
      "ok"
    (error (symbol-name (car err)))))

;; condition-case-unless-debug (same inference)
(defun cc-unless-debug ()
  :string:
  (condition-case-unless-debug nil
      "ok"
    (error "fallback")))

;; Multiple handler clauses
(defun cc-multi-handler ()
  (declare (tart () -> (string | int | nil)))
  (condition-case nil
      "ok"
    (arith-error 42)
    (void-function nil)))

;; Nil variable (no binding in handlers)
(defun cc-nil-var ()
  :int:
  (condition-case nil
      (+ 1 2)
    (arith-error 0)))

;; Handler with body sequence (last expression is handler type)
(defun cc-handler-body-seq ()
  :string:
  (condition-case nil
      "ok"
    (error
      (message "logging")
      "recovered")))
