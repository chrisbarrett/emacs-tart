(** Shared state between lexer and parser *)

(** Current file being parsed *)
let filename = ref "<input>"

(** Set the current filename *)
let set_filename name = filename := name

(** Get the current filename *)
let get_filename () = !filename
