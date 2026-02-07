;; test: emacs-version 31.0
;;
;; Spec 57 R5: Clause without diagnostic matches; no diagnostic emitted.
;; When functionp is called with a function type, the first clause matches
;; (which has no diagnostic), so no clause diagnostic appears.

(defun test-fn (f)
  (declare (tart (((&rest any) -> any)) -> t))
  (functionp f))
