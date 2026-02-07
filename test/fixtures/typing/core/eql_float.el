;; eql with float args: float is eql-safe (numeric equality)
;; test: emacs-version 31.0

;; float is eql-safe because eql uses = for numbers
(defun eql-floats (a b)
  (declare (tart (float float) -> bool))
  (eql a b))

;; int is also eql-safe
(defun eql-ints (a b)
  (declare (tart (int int) -> bool))
  (eql a b))
