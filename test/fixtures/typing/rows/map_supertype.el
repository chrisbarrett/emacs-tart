;; R12: map supertype — function typed with map accepts alist
;; A map-typed parameter accepts a row-typed alist via subtyping.
;; test: emacs-version 31.0

(defun get-id (x)
  (declare (tart [r] ((map {id int & r}) -> int)))
  (map-elt x 'id))
