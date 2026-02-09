;; tart-macros.el --- Curated macro definitions for type-checker expansion
;;
;; Simplified macros that rewrite defining forms into forms the type checker
;; already understands (defun, defvar, let, progn).  Loaded by the macro
;; expansion pre-pass in module_check.ml.
;;
;; ORDER MATTERS: `defmacro` macro must be defined LAST so that
;; `load_macros` processes all other macros via the special-form path first.

;; ---------------------------------------------------------------------------
;; Defun-like forms — rewrite to (defun name arglist &rest body)
;; ---------------------------------------------------------------------------

;; defsubst: identical structure to defun
(defmacro defsubst (name arglist &rest body)
  `(defun ,name ,arglist ,@body))

;; cl-defgeneric: (cl-defgeneric name arglist [docstring] &rest options)
;; Drop everything after arglist except docstring
(defmacro cl-defgeneric (name arglist &rest body)
  (if (and body (stringp (car body)))
      `(defun ,name ,arglist ,(car body) nil)
    `(defun ,name ,arglist nil)))

;; cl-defmethod: skip qualifiers and strip type specializers from arglist
;; (cl-defmethod name [:before|:after|:around|:extra "str"] arglist &rest body)
(defmacro cl-defmethod (name &rest rest)
  ;; Skip keyword qualifiers and their string arguments
  (let ((args rest))
    (while (and args (keywordp (car args)))
      ;; Skip the keyword
      (setq args (cdr args))
      ;; If :extra, also skip the string argument
      (if (and args (stringp (car args)))
          (setq args (cdr args))))
    (if args
        (let ((raw-arglist (car args))
              (body (cdr args)))
          ;; Strip type specializers from arglist: (arg type) -> arg
          (let ((clean-arglist
                 (mapcar (lambda (param)
                           (if (listp param)
                               (car param)
                             param))
                         raw-arglist)))
            `(defun ,name ,clean-arglist ,@body)))
      ;; Fallback: no arglist found
      `(defvar ,name nil))))

;; pcase-defmacro: (pcase-defmacro name arglist &rest body)
(defmacro pcase-defmacro (name arglist &rest body)
  `(defun ,name ,arglist ,@body))

;; gv-define-setter: (gv-define-setter name arglist &rest body)
(defmacro gv-define-setter (name arglist &rest body)
  `(defun ,name ,arglist ,@body))

;; gv-define-expander: (gv-define-expander name handler)
(defmacro gv-define-expander (name &rest body)
  `(defun ,name (&rest args) ,@body))

;; ---------------------------------------------------------------------------
;; Variable-binding forms — rewrite to (defvar name value)
;; ---------------------------------------------------------------------------

;; defcustom: (defcustom name value docstring &rest args)
(defmacro defcustom (name value &rest rest)
  `(defvar ,name ,value))

;; defgroup: (defgroup name value docstring &rest args)
(defmacro defgroup (name value &rest rest)
  `(defvar ,name ,value))

;; defface: (defface name value docstring &rest args)
(defmacro defface (name value &rest rest)
  `(defvar ,name ,value))

;; declare-function: (declare-function name file &optional arglist fileonly)
;; Just make the name known as a variable
(defmacro declare-function (name &rest rest)
  `(defvar ,name nil))

;; cl--defalias: (cl--defalias name definition &optional docstring)
(defmacro cl--defalias (name &rest rest)
  `(defvar ,name nil))

;; ---------------------------------------------------------------------------
;; Dual-binding forms
;; ---------------------------------------------------------------------------

;; define-minor-mode: creates both a variable and a toggle function
;; (define-minor-mode name docstring &rest body)
(defmacro define-minor-mode (name &rest body)
  `(progn
     (defvar ,name nil)
     (defun ,name (&rest args) ,@body)))

;; ---------------------------------------------------------------------------
;; Local binding forms — rewrite to let
;; ---------------------------------------------------------------------------

;; gv-letplace: (gv-letplace (getter setter) place &rest body)
(defmacro gv-letplace (bindings place &rest body)
  (let ((getter (car bindings))
        (setter (car (cdr bindings))))
    `(let ((,getter ,place)
           (,setter nil))
       ,@body)))

;; macroexp-let2: (macroexp-let2 [test] var exp &rest body)
;; Simplified: just bind var to exp in body
(defmacro macroexp-let2 (&rest args)
  ;; First arg might be a symbol like `macroexp-copyable-p` — skip it
  ;; Pattern: (macroexp-let2 [TEST] SYM EXP BODY...)
  (if (symbolp (car (cdr args)))
      ;; (macroexp-let2 test var exp body...) - skip test
      (let ((var (car (cdr args)))
            (exp (car (cdr (cdr args))))
            (body (cdr (cdr (cdr args)))))
        `(let ((,var ,exp)) ,@body))
    ;; (macroexp-let2 var exp body...) - no test
    (let ((var (car args))
          (exp (car (cdr args)))
          (body (cdr (cdr args))))
      `(let ((,var ,exp)) ,@body))))

;; ---------------------------------------------------------------------------
;; No-op forms
;; ---------------------------------------------------------------------------

;; set-advertised-calling-convention: metadata only, discard
(defmacro set-advertised-calling-convention (&rest args)
  nil)

;; ---------------------------------------------------------------------------
;; defmacro itself — defined LAST (see file header comment)
;; Treat as defun for type checking purposes
;; ---------------------------------------------------------------------------

(defmacro defmacro (name arglist &rest body)
  `(defun ,name ,arglist ,@body))
