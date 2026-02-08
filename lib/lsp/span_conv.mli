(** Span-to-LSP conversion utilities.

    Converts {!Syntax.Location.span} values to {!Protocol.range} and
    {!Protocol.location}, performing 1-based→0-based line adjustment and
    byte→UTF-16 column conversion. *)

val range_of_span : text:string -> Syntax.Location.span -> Protocol.range
(** [range_of_span ~text span] converts a source span with 1-based lines and
    byte columns to a 0-based LSP range with UTF-16 character offsets. *)

val location_of_span : text:string -> Syntax.Location.span -> Protocol.location
(** [location_of_span ~text span] converts a source span to an LSP location,
    using the span's filename as a [file://] URI. *)
