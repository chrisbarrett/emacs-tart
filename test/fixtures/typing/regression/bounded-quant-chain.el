;; Bounded quantification: chained inference (Spec 87)
;; test: emacs-version 31.0
;;
;; When a function passes its inferred param to a rest-param union function,
;; the bound is captured during generalization. Callers of the wrapper can
;; pass valid subtypes of the bound.

;; Base: function with rest-param union
(defun tagged-log (tag &rest args)
  (declare (tart (string &rest (string | symbol)) -> string))
  tag)

;; Wrapper: passes its param through to rest-param union fn
;; Inferred type: x acquires bound <: (string | symbol)
(defun log-one (x)
  (tagged-log "info" x))

;; Case 1: call wrapper with string — pass
(defun use-string ()
  (declare (tart () -> string))
  (log-one "hello"))

;; Case 2: call wrapper with symbol — pass
(defun use-symbol ()
  (declare (tart () -> string))
  (log-one 'world))
