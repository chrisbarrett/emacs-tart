(** ANSI color codes and TTY detection for terminal output.

    Provides helpers for colorized terminal output that gracefully degrades to
    plain text when stdout is not a TTY (e.g., when piped). *)

val init : unit -> unit
(** Initialize color detection. Call once at startup. *)

val force_colors : bool -> unit
(** Force colors on or off regardless of TTY detection. *)

val use_colors : unit -> bool
(** Check if colors should be used. *)

(** {1 Semantic Colors}

    Colors for error output per Spec 45 R11. *)

val error : string -> string
(** Red bold - for "error" severity label *)

val warning : string -> string
(** Yellow bold - for "warning" severity label *)

val hint : string -> string
(** Cyan - for "hint" severity label *)

val error_code : string -> string
(** Red - for error codes like [E0001] *)

val location : string -> string
(** Blue - for --> location arrows and file paths *)

val line_number : string -> string
(** Blue dim - for line number gutter *)

val underline : string -> string
(** Red - for ^^^^ underline carets *)

val type_name : string -> string
(** Green - for type names in messages *)

val help : string -> string
(** Cyan - for help text *)

(** {1 Syntax Highlighting}

    Colors for Lisp syntax highlighting per Spec 45 R12. *)

val keyword : string -> string
(** Magenta - for defun, let, if, etc. *)

val string_lit : string -> string
(** Green - for string literals *)

val comment : string -> string
(** Gray/dim - for ;; comments *)

val number : string -> string
(** Cyan - for numeric literals *)

val quoted : string -> string
(** Yellow - for 'symbol quotes *)
