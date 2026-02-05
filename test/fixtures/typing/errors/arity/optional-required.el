;; Arity error: omitting required arg when optional exists
;; Error: aref requires both vector and index arguments
;; test: emacs-version 31.0

(defun broken-array-access ()
  "Call aref with only the vector argument, missing index."
  (aref [1 2 3]))
