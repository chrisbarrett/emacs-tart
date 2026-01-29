;; Realistic user scenario: using function without require
;;
;; This fixture represents a common mistake in user configuration:
;; calling a function from a package without requiring it first.
;; The function appears unbound because the dependency isn't loaded.

(defun my-setup ()
  "Set up my configuration."
  ;; seq-filter is from seq.el but we forgot (require 'seq)
  (seq-filter #'numberp (list 1 "two" 3)))
