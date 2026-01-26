(** Tests for forall quantifier inference *)

open Sig

(** Helper to parse a defun declaration from a string *)
let parse_defun_str s =
  let parse_result = Syntax.Read.parse_string s in
  match parse_result.sexps with
  | [ sexp ] -> (
      match Sig_parser.parse_decl sexp with
      | Ok (Sig_ast.DDefun d) -> Ok d
      | Ok _ -> Error "Expected defun declaration"
      | Error e -> Error e.message)
  | _ -> Error "Expected single expression"

(** Helper to parse a type declaration from a string *)
let parse_type_str s =
  let parse_result = Syntax.Read.parse_string s in
  match parse_result.sexps with
  | [ sexp ] -> (
      match Sig_parser.parse_decl sexp with
      | Ok (Sig_ast.DType d) -> Ok d
      | Ok _ -> Error "Expected type declaration"
      | Error e -> Error e.message)
  | _ -> Error "Expected single expression"

(** Helper to parse a complete signature file *)
let parse_sig_str s =
  let parse_result = Syntax.Read.parse_string s in
  match Sig_parser.parse_signature ~module_name:"test" parse_result.sexps with
  | Ok sig_file -> Ok sig_file
  | Error errors ->
      let msgs = List.map (fun e -> e.Sig_parser.message) errors in
      Error (String.concat "; " msgs)

(** Helper to get binder names *)
let binder_names (binders : Sig_ast.tvar_binder list) : string list =
  List.map (fun b -> b.Sig_ast.name) binders

(** {1 R1: Implicit Quantification} *)

let test_basic_inference () =
  (* (defun identity (a) -> a) should infer [a] *)
  match parse_defun_str "(defun identity (a) -> a)" with
  | Error e -> Alcotest.fail e
  | Ok d ->
      let d' = Forall_infer.infer_defun ~known_types:[] d in
      Alcotest.(check (list string))
        "inferred [a]" [ "a" ]
        (binder_names d'.defun_tvar_binders)

let test_seq_map_inference () =
  (* (defun seq-map (((a -> b)) (seq a)) -> (list b)) should infer [a b] *)
  match parse_defun_str "(defun seq-map (((a -> b)) (seq a)) -> (list b))" with
  | Error e -> Alcotest.fail e
  | Ok d ->
      let d' = Forall_infer.infer_defun ~known_types:[] d in
      Alcotest.(check (list string))
        "inferred [a b]" [ "a"; "b" ]
        (binder_names d'.defun_tvar_binders)

let test_first_occurrence_order () =
  (* (defun compose (((b -> c)) ((a -> b))) -> ((a -> c))) should infer [b c a] *)
  match
    parse_defun_str "(defun compose (((b -> c)) ((a -> b))) -> ((a -> c)))"
  with
  | Error e -> Alcotest.fail e
  | Ok d ->
      let d' = Forall_infer.infer_defun ~known_types:[] d in
      Alcotest.(check (list string))
        "inferred [b c a] in first-occurrence order" [ "b"; "c"; "a" ]
        (binder_names d'.defun_tvar_binders)

(** {1 R2: Explicit Quantification Disables Inference} *)

let test_explicit_disables_inference () =
  (* (defun identity [a] (a) -> a) already has explicit quantifier - no change *)
  match parse_defun_str "(defun identity [a] (a) -> a)" with
  | Error e -> Alcotest.fail e
  | Ok d ->
      let d' = Forall_infer.infer_defun ~known_types:[] d in
      Alcotest.(check (list string))
        "explicit [a] unchanged" [ "a" ]
        (binder_names d'.defun_tvar_binders)

let test_explicit_with_missing_var () =
  (* (defun foo [a] ((a -> b)) -> a) has explicit [a] but uses unbound b.
     Inference doesn't add b - validation will catch this. *)
  match parse_defun_str "(defun foo [a] ((a -> b)) -> a)" with
  | Error e -> Alcotest.fail e
  | Ok d ->
      let d' = Forall_infer.infer_defun ~known_types:[] d in
      (* Should not change - explicit quantifier present *)
      Alcotest.(check (list string))
        "explicit [a] not modified" [ "a" ]
        (binder_names d'.defun_tvar_binders)

(** {1 R3: Phantom Type Variables} *)

let test_phantom_in_return () =
  (* (defun make-tagged int -> (tagged tag int)) should infer [tag] from return type *)
  match parse_defun_str "(defun make-tagged (int) -> (tagged tag int))" with
  | Error e -> Alcotest.fail e
  | Ok d ->
      let d' = Forall_infer.infer_defun ~known_types:[] d in
      Alcotest.(check (list string))
        "inferred [tag] from return type" [ "tag" ]
        (binder_names d'.defun_tvar_binders)

(** {1 R4: Known Types Not Inferred} *)

let test_known_type_not_variable () =
  (* If 'seq' is a known type, it should not be inferred as a type variable *)
  match parse_defun_str "(defun get-seq () -> seq)" with
  | Error e -> Alcotest.fail e
  | Ok d ->
      let d' = Forall_infer.infer_defun ~known_types:[ "seq" ] d in
      Alcotest.(check (list string))
        "seq not inferred (known type)" []
        (binder_names d'.defun_tvar_binders)

let test_primitives_not_inferred () =
  (* Primitives like int, string should not be inferred *)
  match parse_defun_str "(defun add (int int) -> int)" with
  | Error e -> Alcotest.fail e
  | Ok d ->
      let d' = Forall_infer.infer_defun ~known_types:[] d in
      Alcotest.(check (list string))
        "no type variables inferred" []
        (binder_names d'.defun_tvar_binders)

(** {1 R5: Nested Arrow Types} *)

let test_nested_arrows () =
  (* (defun apply (((a -> b)) a) -> b) - variables in nested arrow *)
  match parse_defun_str "(defun apply (((a -> b)) a) -> b)" with
  | Error e -> Alcotest.fail e
  | Ok d ->
      let d' = Forall_infer.infer_defun ~known_types:[] d in
      Alcotest.(check (list string))
        "variables collected from nested arrow" [ "a"; "b" ]
        (binder_names d'.defun_tvar_binders)

let test_deeply_nested_arrows () =
  (* (defun curry ((((a b) -> c)) a b) -> c) - deeply nested *)
  match parse_defun_str "(defun curry ((((a b) -> c)) a b) -> c)" with
  | Error e -> Alcotest.fail e
  | Ok d ->
      let d' = Forall_infer.infer_defun ~known_types:[] d in
      Alcotest.(check (list string))
        "variables from deeply nested arrows" [ "a"; "b"; "c" ]
        (binder_names d'.defun_tvar_binders)

(** {1 R6: Deduplication} *)

let test_deduplication () =
  (* (defun seq-find (((a -> bool)) (seq a)) -> (option a))
     'a' appears multiple times but should only be in quantifier once *)
  match
    parse_defun_str "(defun seq-find (((a -> bool)) (seq a)) -> (option a))"
  with
  | Error e -> Alcotest.fail e
  | Ok d ->
      let d' = Forall_infer.infer_defun ~known_types:[] d in
      Alcotest.(check (list string))
        "single [a] despite multiple occurrences" [ "a" ]
        (binder_names d'.defun_tvar_binders)

(** {1 Type Declaration Inference} *)

let test_type_alias_inference () =
  (* (type int-pair (pair a a)) should infer [a] *)
  match parse_type_str "(type int-pair (pair a a))" with
  | Error e -> Alcotest.fail e
  | Ok d ->
      let d' = Forall_infer.infer_type_decl ~known_types:[] d in
      Alcotest.(check (list string))
        "inferred [a] for type alias" [ "a" ]
        (binder_names d'.type_params)

let test_type_explicit_params () =
  (* (type result [a e] ((ok a) | (err e))) has explicit params - no change *)
  match parse_type_str "(type result [a e] ((ok a) | (err e)))" with
  | Error e -> Alcotest.fail e
  | Ok d ->
      let d' = Forall_infer.infer_type_decl ~known_types:[] d in
      Alcotest.(check (list string))
        "explicit [a e] unchanged" [ "a"; "e" ]
        (binder_names d'.type_params)

let test_opaque_type_no_inference () =
  (* (type buffer) - opaque type, nothing to infer *)
  match parse_type_str "(type buffer)" with
  | Error e -> Alcotest.fail e
  | Ok d ->
      let d' = Forall_infer.infer_type_decl ~known_types:[] d in
      Alcotest.(check (list string))
        "opaque type has no inferred params" []
        (binder_names d'.type_params)

(** {1 Signature-Level Inference} *)

let test_signature_inference () =
  (* Signature with multiple declarations - types become known *)
  let src =
    {|
    (type seq [a])
    (defun seq-map (((a -> b)) (seq a)) -> (seq b))
  |}
  in
  match parse_sig_str src with
  | Error e -> Alcotest.fail e
  | Ok sig_file -> (
      let sig_file' = Forall_infer.infer_signature sig_file in
      match sig_file'.sig_decls with
      | [ _; Sig_ast.DDefun d ] ->
          (* 'seq' is a known type, so only a and b are inferred *)
          Alcotest.(check (list string))
            "seq not inferred (known type in sig)" [ "a"; "b" ]
            (binder_names d.defun_tvar_binders)
      | _ -> Alcotest.fail "Expected type and defun declarations")

(** {1 Integration: Loaded Signature Has Polymorphic Type} *)

let test_loaded_signature_polymorphic () =
  (* When loaded, inference should produce a polymorphic scheme *)
  let src = "(defun identity (a) -> a)" in
  match parse_sig_str src with
  | Error e -> Alcotest.fail e
  | Ok sig_file -> (
      let env = Sig_loader.load_signature Core.Type_env.empty sig_file in
      match Core.Type_env.lookup "identity" env with
      | Some (Core.Type_env.Poly (vars, _constraints, _)) ->
          Alcotest.(check (list string))
            "identity is polymorphic with [a]" [ "a" ] vars
      | Some (Core.Type_env.Mono _) ->
          Alcotest.fail "Expected polymorphic scheme, got monomorphic"
      | None -> Alcotest.fail "identity not found in environment")

let test_loaded_seq_map_polymorphic () =
  (* seq-map should be polymorphic after loading *)
  let src = "(defun seq-map (((a -> b)) (list a)) -> (list b))" in
  match parse_sig_str src with
  | Error e -> Alcotest.fail e
  | Ok sig_file -> (
      let env = Sig_loader.load_signature Core.Type_env.empty sig_file in
      match Core.Type_env.lookup "seq-map" env with
      | Some (Core.Type_env.Poly (vars, _constraints, _)) ->
          Alcotest.(check (list string))
            "seq-map is polymorphic with [a b]" [ "a"; "b" ] vars
      | Some (Core.Type_env.Mono _) ->
          Alcotest.fail "Expected polymorphic scheme, got monomorphic"
      | None -> Alcotest.fail "seq-map not found in environment")

let () =
  Alcotest.run "forall_infer"
    [
      ( "r1-implicit-quantification",
        [
          Alcotest.test_case "basic inference" `Quick test_basic_inference;
          Alcotest.test_case "seq-map inference" `Quick test_seq_map_inference;
          Alcotest.test_case "first-occurrence order" `Quick
            test_first_occurrence_order;
        ] );
      ( "r2-explicit-disables-inference",
        [
          Alcotest.test_case "explicit disables inference" `Quick
            test_explicit_disables_inference;
          Alcotest.test_case "explicit with missing var" `Quick
            test_explicit_with_missing_var;
        ] );
      ( "r3-phantom-types",
        [
          Alcotest.test_case "phantom in return type" `Quick
            test_phantom_in_return;
        ] );
      ( "r4-known-types",
        [
          Alcotest.test_case "known type not variable" `Quick
            test_known_type_not_variable;
          Alcotest.test_case "primitives not inferred" `Quick
            test_primitives_not_inferred;
        ] );
      ( "r5-nested-arrows",
        [
          Alcotest.test_case "nested arrows" `Quick test_nested_arrows;
          Alcotest.test_case "deeply nested arrows" `Quick
            test_deeply_nested_arrows;
        ] );
      ( "r6-deduplication",
        [ Alcotest.test_case "deduplication" `Quick test_deduplication ] );
      ( "type-decl-inference",
        [
          Alcotest.test_case "type alias inference" `Quick
            test_type_alias_inference;
          Alcotest.test_case "type explicit params" `Quick
            test_type_explicit_params;
          Alcotest.test_case "opaque type no inference" `Quick
            test_opaque_type_no_inference;
        ] );
      ( "signature-level",
        [
          Alcotest.test_case "signature inference" `Quick
            test_signature_inference;
        ] );
      ( "integration-loaded",
        [
          Alcotest.test_case "loaded identity polymorphic" `Quick
            test_loaded_signature_polymorphic;
          Alcotest.test_case "loaded seq-map polymorphic" `Quick
            test_loaded_seq_map_polymorphic;
        ] );
    ]
