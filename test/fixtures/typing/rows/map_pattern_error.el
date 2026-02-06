;; R6: map pattern type error — field type flows through to body
;; When the map has {name string & r}, extracting :name gives string.
;; Using it as int should produce a type error.
;; test: emacs-version 31.0

(defun bad-use (person)
  (declare (tart [r] ((map {name string & r}) -> int)))
  (pcase-let (((map :name) person))
    (tart int name)))
