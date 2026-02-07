;; R8: Row type inferred from literal-key map-elt
;; No type annotation on m — its row type is inferred from field access.
;; The 1+ call constrains the field type to num.
;; test: emacs-version 31.0

(defun get-id-next (m)
  (1+ (map-elt m 'id)))
