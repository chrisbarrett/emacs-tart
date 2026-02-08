(** Type hierarchy for the LSP server.

    Provides supertype and subtype relationships for types, triggered by
    [typeHierarchy/prepare], [typeHierarchy/supertypes], and
    [typeHierarchy/subtypes] requests.

    Subtype relationships come from:
    - Numeric tower: [Int <: Num], [Float <: Num]
    - Union types: each member is a subtype of the union *)

type type_info = { name : string; display_name : string }
(** A type with its internal name and display name. *)

let supertypes_of (tcon_name : string) : type_info list =
  let open Core.Types in
  if tcon_name = Prim.int_name || tcon_name = Prim.float_name then
    [ { name = Prim.num_name; display_name = "Num" } ]
  else []

let subtypes_of (tcon_name : string) : type_info list =
  let open Core.Types in
  if tcon_name = Prim.num_name then
    [
      { name = Prim.int_name; display_name = "Int" };
      { name = Prim.float_name; display_name = "Float" };
    ]
  else []
