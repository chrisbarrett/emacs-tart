;; R7: Row-polymorphic map types permit extra fields
;;
;; An open row {name string & r} accepts values with additional fields
;; beyond those specified. This contrasts with unions which are closed.
;; test: emacs-version 31.0

;; Function expecting at least a 'name field
(defun get-name (person)
  (declare (tart [r] ((alist {name string & r}) -> string)))
  (alist-get 'name person))

;; Caller passes alist with extra 'age field — open row accepts it
(defun get-name-from-record (x)
  (declare (tart [r] ((alist {name string age int & r}) -> string)))
  (get-name x))
