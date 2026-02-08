(** URI handling for file:// URIs. *)

let file_scheme = "file://"
let file_scheme_len = String.length file_scheme

let to_filename (uri : string) : string =
  if
    String.length uri > file_scheme_len
    && String.sub uri 0 file_scheme_len = file_scheme
  then String.sub uri file_scheme_len (String.length uri - file_scheme_len)
  else uri

let of_filename (path : string) : string = file_scheme ^ path
