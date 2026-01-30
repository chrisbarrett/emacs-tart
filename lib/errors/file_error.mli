(** Structured file I/O error handling with suggestions. *)

(** File error types with structured data for formatting. *)
type t =
  | File_not_found of { path : string; suggestions : string list }
  | Permission_denied of { path : string }
  | Is_directory of { path : string }
  | Read_error of { path : string; message : string }
  | Signature_not_found of {
      module_name : string;
      search_paths : string list;
      span : Syntax.Location.span option;
    }

val error_code : t -> string
(** Error code for each file error type (E0001-E0005). *)

val levenshtein : string -> string -> int
(** Compute Levenshtein edit distance between two strings. *)

val find_similar_files : max_distance:int -> string -> string -> string list
(** Find similar filenames in a directory using Levenshtein distance.

    [find_similar_files ~max_distance dir basename] returns filenames in [dir]
    with edit distance <= [max_distance] from [basename], sorted by distance. *)

val suggest_el_extension : string -> string option
(** Check if adding .el extension would match an existing file.

    Returns [Some path_with_el] if the file exists, [None] otherwise. *)

val build_suggestions : string -> string list
(** Build suggestions for a file not found error.

    Checks for missing .el extension and similar filenames. *)

val check_file : string -> (unit, t) result
(** Attempt to open a file and classify any error.

    Returns [Ok ()] if the file exists and is readable, or [Error t] with a
    structured file error. *)

val to_string : t -> string
(** Format a file error as a human-readable string. *)

val to_json : t -> Yojson.Safe.t
(** Serialize a file error to JSON. *)

val path_of : t -> string
(** Get the path associated with a file error. *)
