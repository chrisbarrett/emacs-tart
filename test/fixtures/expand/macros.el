;; Macro definitions for testing --load functionality

(defmacro when (test &rest body)
  `(if ,test (progn ,@body)))

(defmacro unless (test &rest body)
  `(if ,test nil (progn ,@body)))

(defmacro inc (var)
  `(setq ,var (1+ ,var)))
