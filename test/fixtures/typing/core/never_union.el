;; Never-union simplification test fixture
;; Tests: (T | never) simplifies to T — never is stripped from unions
;; test: emacs-version 31.0

;; if with error in else: (string | never) simplifies to string
(defun never-stripped-from-if (x)
  :string:
  (if x "ok" (error "fail")))

;; cond with error fallthrough: never stripped from resulting union
(defun never-stripped-from-cond (x)
  :int:
  (cond
    (x 42)
    (t (error "unreachable"))))

;; subtract_type returns never, used in narrowed else-branch
;; when (stringp x) is true, else-branch narrows x to (any - string),
;; which should not contain never
(defun narrowed-else-no-never (x)
  :string:
  (if (stringp x) x "default"))
