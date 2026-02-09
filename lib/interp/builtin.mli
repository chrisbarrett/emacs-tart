(** Pure built-in functions for the Elisp interpreter.

    This module provides implementations of core Elisp functions that are pure
    (no side effects beyond the interpreter's state). Functions that require
    I/O, buffers, or other Emacs-specific state are not included and hit opaque
    boundaries.

    Higher-order functions (mapcar, mapc, apply, funcall) require access to the
    evaluator and are implemented as special forms in {!Eval}. *)

(** {1 Built-in registry} *)

val builtins : Value.value list
(** All pure built-in functions as [Value.Builtin] values. *)

val init_globals : Env.global -> unit
(** Initialize a global environment with all built-in functions. Also adds [nil]
    and [t] as global bindings. *)

(** {1 Individual built-ins}

    These are exposed for testing. Each function takes a list of arguments and
    returns [Ok value] or [Error message]. *)

(** {2 List operations} *)

val car : Value.value list -> (Value.value, string) result
val cdr : Value.value list -> (Value.value, string) result
val cons : Value.value list -> (Value.value, string) result
val list : Value.value list -> (Value.value, string) result
val length : Value.value list -> (Value.value, string) result
val nth : Value.value list -> (Value.value, string) result
val nthcdr : Value.value list -> (Value.value, string) result
val append : Value.value list -> (Value.value, string) result
val reverse : Value.value list -> (Value.value, string) result
val member : Value.value list -> (Value.value, string) result
val memq : Value.value list -> (Value.value, string) result
val assoc : Value.value list -> (Value.value, string) result
val assq : Value.value list -> (Value.value, string) result

(** {2 Arithmetic} *)

val plus : Value.value list -> (Value.value, string) result
val minus : Value.value list -> (Value.value, string) result
val times : Value.value list -> (Value.value, string) result
val divide : Value.value list -> (Value.value, string) result
val modulo : Value.value list -> (Value.value, string) result
val abs_ : Value.value list -> (Value.value, string) result
val max_ : Value.value list -> (Value.value, string) result
val min_ : Value.value list -> (Value.value, string) result

(** {2 Comparisons} *)

val lt : Value.value list -> (Value.value, string) result
val gt : Value.value list -> (Value.value, string) result
val le : Value.value list -> (Value.value, string) result
val ge : Value.value list -> (Value.value, string) result
val eq_num : Value.value list -> (Value.value, string) result

(** {2 Predicates} *)

val null_ : Value.value list -> (Value.value, string) result
val atom : Value.value list -> (Value.value, string) result
val listp : Value.value list -> (Value.value, string) result
val consp : Value.value list -> (Value.value, string) result
val symbolp : Value.value list -> (Value.value, string) result
val stringp : Value.value list -> (Value.value, string) result
val numberp : Value.value list -> (Value.value, string) result
val integerp : Value.value list -> (Value.value, string) result
val floatp : Value.value list -> (Value.value, string) result
val vectorp : Value.value list -> (Value.value, string) result
val functionp : Value.value list -> (Value.value, string) result
val keywordp : Value.value list -> (Value.value, string) result
val eq_ : Value.value list -> (Value.value, string) result
val equal_ : Value.value list -> (Value.value, string) result
val not_ : Value.value list -> (Value.value, string) result

(** {2 String operations} *)

val concat : Value.value list -> (Value.value, string) result
val substring : Value.value list -> (Value.value, string) result
val string_length : Value.value list -> (Value.value, string) result
val upcase : Value.value list -> (Value.value, string) result
val downcase : Value.value list -> (Value.value, string) result
val string_to_list : Value.value list -> (Value.value, string) result
val format_ : Value.value list -> (Value.value, string) result

(** {2 Vector operations} *)

val vector : Value.value list -> (Value.value, string) result
val aref : Value.value list -> (Value.value, string) result
val aset : Value.value list -> (Value.value, string) result

(** {2 Symbol operations} *)

val symbol_name : Value.value list -> (Value.value, string) result

(** {2 Type coercion} *)

val number_to_string : Value.value list -> (Value.value, string) result
val string_to_number : Value.value list -> (Value.value, string) result
