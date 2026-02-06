;; R8: Row type inferred from literal-key gethash
;; No type annotation on tbl — its row type is inferred from field access.
;; The 1+ call constrains the field type to num.
;; test: emacs-version 31.0

(defun get-age-next-year (tbl)
  (1+ (gethash 'age tbl)))
