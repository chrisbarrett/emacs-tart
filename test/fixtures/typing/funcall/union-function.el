;; Test: union function types in funcall (Spec 34 R11)
;; test: emacs-version 31.0
;;
;; When the function expression has a union of arrow types, funcall
;; checks args against all variants and returns the union of returns.

;; Compatible union: both variants accept int and return int
(defvar inc-or-dec nil)
(tart-declare inc-or-dec (((int) -> int) | ((int) -> int)))

;; Should work: int arg satisfies both variants, result is int (deduped)
(1+ (funcall inc-or-dec 5))

;; Compatible union with different returns: both accept int
(defvar int-to-varied nil)
(tart-declare int-to-varied (((int) -> string) | ((int) -> symbol)))

;; Should work: int satisfies both, result is (string | symbol)
(funcall int-to-varied 42)

;; Incompatible union: one expects string, the other expects symbol
(defvar str-or-sym-fn nil)
(tart-declare str-or-sym-fn (((string) -> string) | ((symbol) -> symbol)))

;; Should error: no single value satisfies both string and symbol params
(funcall str-or-sym-fn "hello")
