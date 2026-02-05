;; R8: Multiple plist-get calls on the same parameter infer a multi-field row
;; No type declaration — the row type accumulates fields from each access.
;; test: emacs-version 31.0

(defun summarize (person)
  (+ (plist-get person :age) (plist-get person :score)))
