;; R8: Row type inferred from literal-keyword plist-get
;; No type declaration — the row type for the parameter is inferred
;; from the plist-get call, and the field type from the calling context.
;; test: emacs-version 31.0

(defun get-age-next-year (person)
  (1+ (plist-get person :age)))
