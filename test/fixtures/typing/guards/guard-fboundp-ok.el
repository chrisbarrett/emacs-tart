;; test: emacs-version 31.0
;; fboundp guard makes the named function callable (Spec 49 R2)

(defun fboundp-makes-callable ()
  "fboundp testmod-greet allows calling it in the guard body."
  (when (fboundp 'testmod-greet)
    (testmod-greet "hello")))
