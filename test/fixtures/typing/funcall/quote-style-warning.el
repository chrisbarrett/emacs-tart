;; test: emacs-version 31.0
;;
;; R5: Style warning for regular-quoted symbols in funcall/apply.
;; Using 'name instead of #'name emits a hint.

(defun add-one (n)
  (1+ n))

;; Should emit hint: use #'add-one instead
(funcall 'add-one 5)

;; Should emit hint: use #'+ instead
(apply '+ '(1 2 3))

;; No hint for #'
(funcall #'add-one 5)
