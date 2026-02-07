;; test: emacs-version 31.0
;; featurep guard unlocks non-prefixed names in then-branch (Spec 49 R1)
;; Unguarded call to non-prefixed name is an error.

(defun guarded-when ()
  "featurep in when body — hidden-fn available."
  (when (featurep 'testmod)
    (hidden-fn 42)))

(defun guarded-if-then ()
  "featurep in if then-branch — hidden-fn available."
  (if (featurep 'testmod)
      (hidden-fn 42)
    0))

(defun unguarded-call ()
  "No guard — hidden-fn is undefined."
  (hidden-fn 99))
