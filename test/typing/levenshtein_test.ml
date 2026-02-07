(** Tests for Levenshtein distance and typo suggestions. *)

module Lev = Tart.Levenshtein

(* =============================================================================
   distance: basic properties
   ============================================================================= *)

let test_distance_identity () =
  Alcotest.(check int) "d(s,s) = 0" 0 (Lev.distance "hello" "hello")

let test_distance_empty_identity () =
  Alcotest.(check int) "d(\"\",\"\") = 0" 0 (Lev.distance "" "")

let test_distance_symmetry () =
  let d1 = Lev.distance "kitten" "sitting" in
  let d2 = Lev.distance "sitting" "kitten" in
  Alcotest.(check int) "d(a,b) = d(b,a)" d1 d2

let test_distance_empty_left () =
  Alcotest.(check int) "d(\"\",\"abc\") = 3" 3 (Lev.distance "" "abc")

let test_distance_empty_right () =
  Alcotest.(check int) "d(\"abc\",\"\") = 3" 3 (Lev.distance "abc" "")

(* =============================================================================
   distance: single edits
   ============================================================================= *)

let test_distance_single_insert () =
  Alcotest.(check int) "single insertion" 1 (Lev.distance "abc" "abcd")

let test_distance_single_delete () =
  Alcotest.(check int) "single deletion" 1 (Lev.distance "abcd" "abc")

let test_distance_single_substitute () =
  Alcotest.(check int) "single substitution" 1 (Lev.distance "abc" "axc")

(* =============================================================================
   distance: known pairs
   ============================================================================= *)

let test_distance_kitten_sitting () =
  (* kitten → sitten → sittin → sitting = 3 *)
  Alcotest.(check int) "kitten→sitting" 3 (Lev.distance "kitten" "sitting")

let test_distance_saturday_sunday () =
  (* saturday → sturday → surday → sunday = 3 *)
  Alcotest.(check int) "saturday→sunday" 3 (Lev.distance "saturday" "sunday")

let test_distance_completely_different () =
  Alcotest.(check int) "abc→xyz" 3 (Lev.distance "abc" "xyz")

let test_distance_single_char () =
  Alcotest.(check int) "a→b" 1 (Lev.distance "a" "b")

let test_distance_single_char_same () =
  Alcotest.(check int) "a→a" 0 (Lev.distance "a" "a")

(* =============================================================================
   find_similar_names: threshold scaling
   ============================================================================= *)

let test_similar_short_query () =
  (* Length 1-2: max distance 1 *)
  let result = Lev.find_similar_names ~query:"ab" ~candidates:[ "ac"; "xyz" ] in
  Alcotest.(check (list string)) "short query threshold 1" [ "ac" ] result

let test_similar_short_query_no_match () =
  (* Length 1-2: max distance 1; "xyz" is distance 2 from "ab" *)
  let result = Lev.find_similar_names ~query:"ab" ~candidates:[ "xyz" ] in
  Alcotest.(check (list string)) "short query no match" [] result

let test_similar_medium_query () =
  (* Length 3-5: max distance 2 *)
  let result =
    Lev.find_similar_names ~query:"abcd"
      ~candidates:[ "abce"; "abxy"; "abcdef" ]
  in
  (* "abce" distance 1, "abxy" distance 2, "abcdef" distance 2 *)
  Alcotest.(check (list string))
    "medium query threshold 2"
    [ "abce"; "abxy"; "abcdef" ]
    result

let test_similar_long_query () =
  (* Length 6+: max distance 3 *)
  let result =
    Lev.find_similar_names ~query:"abcdef"
      ~candidates:[ "abcxyz"; "completely-different" ]
  in
  (* "abcxyz" distance 3, "completely-different" distance >> 3 *)
  Alcotest.(check (list string)) "long query threshold 3" [ "abcxyz" ] result

(* =============================================================================
   find_similar_names: sorting
   ============================================================================= *)

let test_similar_sorted_by_distance () =
  let result =
    Lev.find_similar_names ~query:"hello"
      ~candidates:[ "hillo"; "helo"; "hullo" ]
  in
  (* helo: d=1, hillo: d=1, hullo: d=1 — all distance 1, sorted by length *)
  (* helo (4) < hillo (5) = hullo (5) *)
  Alcotest.(check (list string))
    "sorted by distance then length"
    [ "helo"; "hillo"; "hullo" ]
    result

let test_similar_excludes_exact_match () =
  (* Exact match (distance 0) is excluded *)
  let result =
    Lev.find_similar_names ~query:"foo" ~candidates:[ "foo"; "for"; "boo" ]
  in
  Alcotest.(check (list string)) "excludes exact match" [ "for"; "boo" ] result

let test_similar_empty_candidates () =
  let result = Lev.find_similar_names ~query:"foo" ~candidates:[] in
  Alcotest.(check (list string)) "empty candidates" [] result

(* =============================================================================
   suggest_name: best match selection
   ============================================================================= *)

let test_suggest_best_match () =
  (* "stringp" → "stringq" distance 1, "number" distance 5 *)
  let result =
    Lev.suggest_name ~query:"stringp" ~candidates:[ "stringq"; "number" ]
  in
  Alcotest.(check (option string)) "best match" (Some "stringq") result

let test_suggest_none_when_no_match () =
  let result =
    Lev.suggest_name ~query:"ab" ~candidates:[ "xyz"; "qrs"; "tuv" ]
  in
  Alcotest.(check (option string)) "no match returns None" None result

let test_suggest_single_candidate () =
  let result = Lev.suggest_name ~query:"helo" ~candidates:[ "hello" ] in
  Alcotest.(check (option string)) "single candidate" (Some "hello") result

let test_suggest_returns_closest () =
  let result =
    Lev.suggest_name ~query:"hello" ~candidates:[ "hillo"; "helo"; "jello" ]
  in
  (* helo: d=1, hillo: d=1, jello: d=1 — all tied, first by sort wins *)
  Alcotest.(check (option string)) "returns closest" (Some "helo") result

let test_suggest_empty_candidates () =
  let result = Lev.suggest_name ~query:"foo" ~candidates:[] in
  Alcotest.(check (option string)) "empty candidates" None result

(* =============================================================================
   distance: elisp-relevant names (hyphens, underscores)
   ============================================================================= *)

let test_distance_elisp_hyphen () =
  (* Common Elisp typo: missing hyphen *)
  Alcotest.(check int)
    "string-match vs stringmatch" 1
    (Lev.distance "string-match" "stringmatch")

let test_distance_elisp_similar () =
  Alcotest.(check int)
    "string-match vs string-patch" 1
    (Lev.distance "string-match" "string-patch")

(* =============================================================================
   Test Suite
   ============================================================================= *)

let () =
  Alcotest.run "levenshtein"
    [
      ( "distance basics",
        [
          Alcotest.test_case "identity" `Quick test_distance_identity;
          Alcotest.test_case "empty identity" `Quick
            test_distance_empty_identity;
          Alcotest.test_case "symmetry" `Quick test_distance_symmetry;
          Alcotest.test_case "empty left" `Quick test_distance_empty_left;
          Alcotest.test_case "empty right" `Quick test_distance_empty_right;
        ] );
      ( "single edits",
        [
          Alcotest.test_case "insert" `Quick test_distance_single_insert;
          Alcotest.test_case "delete" `Quick test_distance_single_delete;
          Alcotest.test_case "substitute" `Quick test_distance_single_substitute;
        ] );
      ( "known pairs",
        [
          Alcotest.test_case "kitten→sitting" `Quick
            test_distance_kitten_sitting;
          Alcotest.test_case "saturday→sunday" `Quick
            test_distance_saturday_sunday;
          Alcotest.test_case "completely different" `Quick
            test_distance_completely_different;
          Alcotest.test_case "single char" `Quick test_distance_single_char;
          Alcotest.test_case "single char same" `Quick
            test_distance_single_char_same;
        ] );
      ( "find_similar_names",
        [
          Alcotest.test_case "short query" `Quick test_similar_short_query;
          Alcotest.test_case "short no match" `Quick
            test_similar_short_query_no_match;
          Alcotest.test_case "medium query" `Quick test_similar_medium_query;
          Alcotest.test_case "long query" `Quick test_similar_long_query;
          Alcotest.test_case "sorted" `Quick test_similar_sorted_by_distance;
          Alcotest.test_case "excludes exact" `Quick
            test_similar_excludes_exact_match;
          Alcotest.test_case "empty candidates" `Quick
            test_similar_empty_candidates;
        ] );
      ( "suggest_name",
        [
          Alcotest.test_case "best match" `Quick test_suggest_best_match;
          Alcotest.test_case "no match" `Quick test_suggest_none_when_no_match;
          Alcotest.test_case "single candidate" `Quick
            test_suggest_single_candidate;
          Alcotest.test_case "returns closest" `Quick
            test_suggest_returns_closest;
          Alcotest.test_case "empty candidates" `Quick
            test_suggest_empty_candidates;
        ] );
      ( "elisp names",
        [
          Alcotest.test_case "hyphen" `Quick test_distance_elisp_hyphen;
          Alcotest.test_case "similar" `Quick test_distance_elisp_similar;
        ] );
    ]
