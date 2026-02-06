;; R12: map supertype accepts alist subtype
;; An alist-typed value can be passed where map is expected.
;; test: emacs-version 31.0

(defun get-name (x)
  (declare (tart [r] ((map {name string & r}) -> string)))
  (map-elt x 'name))

(defun use-get-name (person)
  (declare (tart [r] ((alist {name string & r}) -> string)))
  (get-name person))
