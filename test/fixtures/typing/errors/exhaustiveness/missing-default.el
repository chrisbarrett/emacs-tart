;; Exhaustiveness: cond without catch-all
;; test: emacs-version 31.0
;;
;; Note: cond exhaustiveness is not currently checked by tart.
;; Exhaustiveness checking is only implemented for pcase with
;; ADT types defined in .tart signature files.
;;
;; This fixture demonstrates a cond without a default/catch-all
;; clause, which could fail to handle all cases.

(defun classify-number (x)
  "Classify a number, but missing catch-all case."
  (cond
   ((= x 0) "zero")
   ((> x 0) "positive")))
