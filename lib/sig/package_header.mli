(** Parse Package-Requires from Emacs Lisp file headers.

    Extracts the minimum Emacs version declared in the Package-Requires header
    of `.el` files. This header is a standard convention in the Emacs package
    ecosystem.

    This implements R4-R7 of Spec 50 (version constraints). *)

val parse_package_requires : string -> Emacs_version.version option
(** [parse_package_requires content] extracts the Emacs version from a
    Package-Requires header in [content].

    Scans the first 50 lines for a line matching:
    {v ;; Package-Requires: ((emacs "X.Y") ...) v}

    Returns [Some version] if found, [None] otherwise.

    Handles variations:
    - Multiple dependencies: [((emacs "28.1") (seq "2.24"))]
    - Emacs-only: [((emacs "29.1"))]
    - Leading semicolons and whitespace *)

val find_package_version : string -> Emacs_version.version option
(** [find_package_version path] reads the `.el` file at [path] and extracts the
    declared Emacs version from its Package-Requires header.

    Returns [None] if the file cannot be read or has no Package-Requires header
    with an emacs dependency. *)
