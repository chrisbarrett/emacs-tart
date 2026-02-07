;; test: emacs-version 31.0
;; boundp unlocks only the named variable (Spec 49 R3)

(defun boundp-exposes-var ()
  "boundp testmod-version makes the variable available."
  (when (boundp 'testmod-version)
    (upcase testmod-version)))

(defun boundp-no-function ()
  "boundp testmod-version does NOT expose hidden-fn."
  (when (boundp 'testmod-version)
    (hidden-fn 42)))
