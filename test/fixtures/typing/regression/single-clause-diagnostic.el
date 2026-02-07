;; Regression: single-clause defun with diagnostic was silently dropped
;; Fixed: iteration 26 (compute_defun_clauses returned None for single-clause)
;; test: emacs-version 31.0

;; A single-clause function with a deprecation warning must still emit
;; the diagnostic when called.
(defun calls-deprecated ()
  (declare (tart () -> bool))
  (interactive-p))
