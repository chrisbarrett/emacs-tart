(** Oracle comparison: tart parser vs Emacs reader.

    Parses input with tart, reads the same input with Emacs, and compares the
    canonical printed forms to detect discrepancies. *)

module Read = Syntax.Read
module Print = Syntax.Print
module Location = Syntax.Location

type comparison_result =
  | Match
  | Mismatch of { tart_output : string; emacs_output : string }
  | Tart_error of Read.parse_error
  | Emacs_error of Emacs_reader.emacs_error

(** {1 Normalisation} *)

let normalise (s : string) : string =
  (* Trim trailing whitespace *)
  let s = String.trim s in
  (* Normalise float representation: Emacs may print 1.0 as 1.0 or 1. *)
  (* We leave this minimal for now; extend as discrepancies are found *)
  s

(** {1 Single-form comparison} *)

let compare_string ?(timeout_ms = 5000) (input : string) : comparison_result =
  (* Parse with tart *)
  let tart_result = Read.parse_one input in
  match tart_result with
  | Error msg -> Tart_error { message = msg; span = Location.dummy_span }
  | Ok sexp -> (
      let tart_output = Print.to_string sexp in
      (* Read with Emacs *)
      match Emacs_reader.read_string ~timeout_ms input with
      | Error e -> Emacs_error e
      | Ok emacs_output ->
          let tart_norm = normalise tart_output in
          let emacs_norm = normalise emacs_output in
          if String.equal tart_norm emacs_norm then Match
          else Mismatch { tart_output = tart_norm; emacs_output = emacs_norm })

(** {1 Multi-form comparison} *)

let compare_file ?(timeout_ms = 5000) (path : string) : comparison_result list =
  (* Parse file with tart *)
  let tart_result = Read.parse_file path in
  let tart_sexps = tart_result.sexps in
  let tart_errors = tart_result.errors in
  (* Read file with Emacs *)
  match Emacs_reader.read_file ~timeout_ms path with
  | Error e -> [ Emacs_error e ]
  | Ok emacs_forms ->
      let tart_strings = List.map Print.to_string tart_sexps in
      let n_tart = List.length tart_strings in
      let n_emacs = List.length emacs_forms in
      (* If tart had parse errors, report them first *)
      let error_results =
        List.map (fun (e : Read.parse_error) -> Tart_error e) tart_errors
      in
      (* Compare form-by-form up to the shorter list *)
      let n_common = min n_tart n_emacs in
      let paired =
        List.init n_common (fun i ->
            let tart_s = normalise (List.nth tart_strings i) in
            let emacs_s = normalise (List.nth emacs_forms i) in
            if String.equal tart_s emacs_s then Match
            else Mismatch { tart_output = tart_s; emacs_output = emacs_s })
      in
      (* Extra tart forms (Emacs saw fewer) are mismatches *)
      let tart_extra =
        if n_tart > n_common then
          List.init (n_tart - n_common) (fun i ->
              Mismatch
                {
                  tart_output = normalise (List.nth tart_strings (n_common + i));
                  emacs_output = "";
                })
        else []
      in
      (* Extra Emacs forms (tart saw fewer) are mismatches *)
      let emacs_extra =
        if n_emacs > n_common then
          List.init (n_emacs - n_common) (fun i ->
              Mismatch
                {
                  tart_output = "";
                  emacs_output = normalise (List.nth emacs_forms (n_common + i));
                })
        else []
      in
      error_results @ paired @ tart_extra @ emacs_extra
