;; Test: apply detects type errors in fixed args
;; test: emacs-version 31.0

;; + expects ints, but we pass a string as a fixed arg
;; (apply #'+ "oops" list) - the "oops" should be an Int
(apply #'+ "oops" '(1 2 3))
