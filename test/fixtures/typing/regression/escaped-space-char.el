;; Regression: backslash-space escape in character literals
;; Fixed: Spec 76 Iteration 4 (validation against Emacs lisp/)
;; The parser crashed on ?\S-\  (Shift+Space) due to unknown escape: \
;; test: emacs-version 31.0

;; Backslash-space should parse as space character (32).
(defun uses-escaped-space ()
  (declare (tart () -> int))
  ?\ )
