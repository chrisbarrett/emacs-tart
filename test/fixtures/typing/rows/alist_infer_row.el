;; R8: Row type inferred from literal-key alist-get
;; No type annotation on person — its row type is inferred from field access.
;; The 1+ call constrains the field type to num.
;; test: emacs-version 31.0

(defun get-age-next-year (person)
  (1+ (alist-get 'age person)))
