;; test: emacs-version 31.0
;; Redundant guard warning: fboundp for a function guaranteed by min version
;; Package-Requires: ((emacs "31.0"))

;; The `length` function is available since Emacs 31.0 (from typings).
;; Since the package declares (emacs "31.0"), guarding with fboundp is
;; redundant and should emit a warning.

(defun redundant-fboundp ()
  "fboundp guard for a function guaranteed by declared version."
  (when (fboundp 'length)
    (length "hello")))
