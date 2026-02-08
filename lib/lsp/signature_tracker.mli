(** Signature tracker for LSP.

    Tracks open .tart signature file buffers. When a .tart file is open in the
    editor, its buffer contents take precedence over disk contents for type
    checking.

    This implements R1, R3 of Spec 26 (track open .tart files, prefer buffer).
*)

type t
(** The signature tracker *)

(** {1 Construction} *)

val create : unit -> t
(** Create an empty signature tracker *)

(** {1 Buffer Management} *)

val set : t -> uri:string -> text:string -> unit
(** Set or update buffer contents for a URI *)

val remove : t -> string -> unit
(** Remove a buffer from tracking (on didClose) *)

val get : t -> string -> string option
(** Get buffer contents by URI, if tracked *)

val mem : t -> string -> bool
(** Check if a URI is being tracked *)

(** {1 Filename/URI Helpers} *)

val filename_of_uri : string -> string
(** @deprecated Use {!Uri.to_filename} instead. *)

val is_tart_file : string -> bool
(** Check if a URI points to a .tart file *)

val module_name_of_uri : string -> string option
(** Get the module name from a .tart URI.

    For "file:///path/to/foo.tart", returns Some "foo". For non-.tart URIs,
    returns None. *)

val get_by_path : t -> string -> string option
(** Get buffer contents by filesystem path.

    Converts the path to a file:// URI and looks it up. *)

(** {1 Dependent .el File Helpers} *)

val sibling_el_uri : string -> string option
(** Get the sibling .el file URI for a .tart URI.

    For "file:///path/to/foo.tart", returns "file:///path/to/foo.el". For
    non-.tart URIs, returns None. *)
