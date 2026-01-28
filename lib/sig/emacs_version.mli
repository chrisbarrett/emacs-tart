(** Emacs version detection.

    Detects the installed Emacs version by running `emacs --version` and parsing
    the output. This is used to load version-appropriate typings.

    This implements R1 and R4 of Spec 24 (version detection). *)

(** {1 Types} *)

type version = { major : int; minor : int; patch : int option }
(** A parsed Emacs version number.

    Examples:
    - {{ major = 31; minor = 0; patch = Some 50 }} for "31.0.50"
    - {{ major = 30; minor = 1; patch = None }} for "30.1" *)

(** Result of version detection *)
type detection_result =
  | Detected of version  (** Successfully detected version *)
  | NotFound  (** No Emacs on PATH *)
  | ParseError of string  (** Emacs found but output couldn't be parsed *)

(** {1 Version String Formatting} *)

val version_to_string : version -> string
(** Format version as "major.minor.patch" or "major.minor" *)

val version_to_dir : version -> string
(** Format version as directory name "major.minor" (no patch) *)

(** {1 Version Parsing} *)

val parse_version : string -> version option
(** Parse a version string like "31.0.50" or "30.1" *)

val parse_emacs_version_output : string -> version option
(** Parse the output of `emacs --version` *)

(** {1 Version Detection} *)

val detect : unit -> detection_result
(** Run `emacs --version` and parse the output.

    Returns:
    - [Detected v] if Emacs is found and version is parsed
    - [NotFound] if Emacs is not on PATH
    - [ParseError msg] if Emacs is found but output is unexpected *)

val detect_or_default : default:version -> unit -> version
(** Detect version with a fallback.

    Returns the detected version, or [default] if detection fails. Useful for
    R4: "No Emacs on PATH + no flag → use latest with warning" *)

(** {1 Version Comparison} *)

val compare_version : version -> version -> int
(** Compare two versions. Returns negative if first is older, positive if first
    is newer, 0 if equal *)

(** {1 Default Versions} *)

val latest : version
(** The "latest" version, currently 31.0. Used as fallback when detection fails.
*)
