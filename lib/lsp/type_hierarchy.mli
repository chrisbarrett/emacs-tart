(** Type hierarchy for the LSP server.

    Provides supertype and subtype relationships for types, triggered by
    [typeHierarchy/prepare], [typeHierarchy/supertypes], and
    [typeHierarchy/subtypes] requests.

    Subtype relationships come from:
    - Numeric tower: [Int <: Num], [Float <: Num]
    - Union types: each member is a subtype of the union *)

type type_info = { name : string; display_name : string }
(** A type with its internal name and display name. *)

val supertypes_of : string -> type_info list
(** [supertypes_of tcon_name] returns the supertypes of the given type constant.
    For [Int] and [Float], returns [Num]. *)

val subtypes_of : string -> type_info list
(** [subtypes_of tcon_name] returns the subtypes of the given type constant. For
    [Num], returns [Int] and [Float]. *)
