;; test: emacs-version 31.0
;; fboundp unlocks only the named function (Spec 49 R2)
;; The function name appears only as quoted symbol, so autoload
;; discovery does not fire.  The guard is the sole mechanism.

(defun fboundp-no-variable ()
  "fboundp for a function does NOT expose variables from the module."
  (when (fboundp 'testmod-greet)
    testmod-version))
