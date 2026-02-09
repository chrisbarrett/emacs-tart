;; define-minor-mode creates both a variable and a function
;; test: emacs-version 31.0

(define-minor-mode my-test-mode
  "A test minor mode.")

;; Mode variable should be known
(defun check-mode-var ()
  (if my-test-mode "on" "off"))
