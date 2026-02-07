;; test: emacs-version 31.0
;; Stored featurep result does NOT unlock names (Spec 49 R17).
;; Guards must be inline — binding the result defeats recognition.

(defun stored-guard-let ()
  "Let-bound featurep does not unlock names."
  (let ((avail (featurep 'testmod)))
    (when avail
      (hidden-fn 42))))

(defun stored-guard-setq ()
  "setq'd featurep does not unlock names."
  (let ((avail nil))
    (setq avail (featurep 'testmod))
    (when avail
      (hidden-fn 42))))
