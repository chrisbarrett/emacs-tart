(** Tart CLI entry point *)

let () =
  let usage =
    "tart [OPTIONS] [FILES]\n\nA type checker for Emacs Lisp.\n\nOptions:"
  in
  let files = ref [] in
  let speclist =
    [
      ( "--version",
        Arg.Unit
          (fun () ->
            print_endline ("tart " ^ Tart.version);
            exit 0),
        " Print version and exit" );
    ]
  in
  Arg.parse speclist (fun f -> files := f :: !files) usage;
  if !files = [] then
    print_endline "tart: no input files. Use --help for usage."
