;; R7: Unions are closed — reject values outside the union
;;
;; A function expecting (string | int) rejects a symbol argument.
;; This demonstrates that union types are closed and don't permit
;; extra type members, unlike open row types.
;; test: emacs-version 31.0

(defun accept-string-or-int (x)
  (declare (tart ((string | int)) -> string))
  (format "%s" x))

(defun bad-caller ()
  (declare (tart () -> string))
  (accept-string-or-int 'foo))
