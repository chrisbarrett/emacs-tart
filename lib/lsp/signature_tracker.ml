(** Signature tracker for LSP.

    Tracks open .tart signature file buffers. When a .tart file is open in the
    editor, its buffer contents take precedence over disk contents for type
    checking. This enables real-time feedback when editing signatures. *)

(** {1 Types} *)

type t = (string, string) Hashtbl.t
(** Mapping from URI to buffer contents *)

(** {1 Construction} *)

let create () : t = Hashtbl.create 16

(** {1 Buffer Management} *)

let set (tracker : t) ~(uri : string) ~(text : string) : unit =
  Hashtbl.replace tracker uri text

let remove (tracker : t) (uri : string) : unit = Hashtbl.remove tracker uri

let get (tracker : t) (uri : string) : string option =
  Hashtbl.find_opt tracker uri

let mem (tracker : t) (uri : string) : bool = Hashtbl.mem tracker uri

(** {1 Filename/URI Helpers} *)

let filename_of_uri (uri : string) : string =
  if String.length uri > 7 && String.sub uri 0 7 = "file://" then
    String.sub uri 7 (String.length uri - 7)
  else uri

let is_tart_file (uri : string) : bool =
  Filename.check_suffix (filename_of_uri uri) ".tart"

(** Get the module name from a .tart URI.

    For "file:///path/to/foo.tart", returns "foo". *)
let module_name_of_uri (uri : string) : string option =
  let filename = filename_of_uri uri in
  if is_tart_file uri then
    Some (Filename.chop_suffix (Filename.basename filename) ".tart")
  else None

(** Get buffer contents by filename path.

    Given a path like "/path/to/foo.tart", looks up the buffer by converting to
    file:// URI format. *)
let get_by_path (tracker : t) (path : string) : string option =
  let uri = "file://" ^ path in
  get tracker uri

(** {1 Dependent .el File Helpers} *)

(** Get the sibling .el file URI for a .tart URI.

    For "file:///path/to/foo.tart", returns "file:///path/to/foo.el". *)
let sibling_el_uri (tart_uri : string) : string option =
  let filename = filename_of_uri tart_uri in
  if is_tart_file tart_uri then
    let el_path = Filename.chop_suffix filename ".tart" ^ ".el" in
    Some ("file://" ^ el_path)
  else None
