;; defcustom expands to defvar — variable should be known
;; test: emacs-version 31.0

(defcustom my-custom-var 42
  "A custom variable."
  :type 'integer
  :group 'my-group)

(defun use-custom ()
  (+ my-custom-var 1))
