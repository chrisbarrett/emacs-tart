(** Levenshtein distance and typo suggestions for undefined name errors.

    Implements edit distance calculation and name suggestion based on
    similarity. Used to provide helpful "did you mean?" suggestions when a
    variable or function name is not found. *)

val distance : string -> string -> int
(** [distance s1 s2] computes the Levenshtein edit distance between two strings.

    The edit distance is the minimum number of single-character edits
    (insertions, deletions, or substitutions) required to transform one string
    into the other. *)

val find_similar_names : query:string -> candidates:string list -> string list
(** [find_similar_names ~query ~candidates] finds names similar to [query].

    Returns candidate names within the edit distance threshold, sorted by
    distance (closest first). The threshold scales with query length:
    - Length 1-2: max distance 1
    - Length 3-5: max distance 2
    - Length 6+: max distance 3 *)

val suggest_name : query:string -> candidates:string list -> string option
(** [suggest_name ~query ~candidates] finds the best single suggestion.

    Returns [Some name] if a good match is found, [None] otherwise. *)
