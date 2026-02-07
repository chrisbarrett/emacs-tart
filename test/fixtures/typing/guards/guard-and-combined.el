;; test: emacs-version 31.0
;; Combined guards via and (Spec 49 R16)

(defun and-featurep-fboundp ()
  "featurep + fboundp combined in and."
  (when (and (featurep 'testmod) (fboundp 'hidden-fn))
    (hidden-fn 42)))

(defun and-guard-predicate (x)
  "featurep guard + stringp predicate combined in and."
  (when (and (featurep 'testmod) (stringp x))
    (hidden-fn (length x))))
