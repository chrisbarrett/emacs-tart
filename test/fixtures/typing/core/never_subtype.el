;; Never bottom type subtyping test fixture
;; Tests: error/signal/throw return never, which is a subtype of any type
;; test: emacs-version 31.0

;; error returns never, which is a subtype of string
(defun error-in-string-context ()
  :string:
  (if t "ok" (error "unreachable")))

;; signal returns never, which is a subtype of int
(defun signal-in-int-context ()
  :int:
  (if t 1 (signal 'error '("bad"))))

;; throw returns never, assignable to string
(defun throw-in-string-context ()
  :string:
  (if t "ok" (throw 'done nil)))

;; never in cond branch
(defun never-in-cond ()
  :string:
  (cond
    ((stringp "a") "yes")
    (t (error "unreachable"))))
