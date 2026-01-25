(lang dune 3.0)
(using menhir 2.1)

(name tart)

(generate_opam_files true)

(source
 (github chrisbarrett/emacs-tart))

(authors "Chris Barrett")

(license GPL-3.0-or-later)

(package
 (name tart)
 (synopsis "A type checker for Emacs Lisp")
 (description "Tart provides type inference and checking for Emacs Lisp code with LSP support.")
 (depends
  (ocaml (>= 5.0))
  (dune (>= 3.0))
  menhir
  ppx_deriving
  yojson
  (alcotest :with-test)))
