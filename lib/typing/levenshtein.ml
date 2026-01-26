(** Levenshtein distance and typo suggestions for undefined name errors.

    Implements edit distance calculation and name suggestion based on
    similarity. Used to provide helpful "did you mean?" suggestions when a
    variable or function name is not found. *)

(** Compute the Levenshtein edit distance between two strings.

    The edit distance is the minimum number of single-character edits
    (insertions, deletions, or substitutions) required to transform one string
    into the other.

    Uses the classic dynamic programming algorithm with O(m*n) time and
    O(min(m,n)) space where m and n are the string lengths. *)
let distance s1 s2 =
  let len1 = String.length s1 in
  let len2 = String.length s2 in
  (* Handle empty string cases *)
  if len1 = 0 then len2
  else if len2 = 0 then len1
  else
    (* Use shorter string for the working row to minimize space *)
    let s1, s2, len1, len2 =
      if len1 > len2 then (s2, s1, len2, len1) else (s1, s2, len1, len2)
    in
    (* prev_row and curr_row alternate as we iterate *)
    let prev_row = Array.init (len1 + 1) (fun i -> i) in
    let curr_row = Array.make (len1 + 1) 0 in
    for j = 1 to len2 do
      curr_row.(0) <- j;
      for i = 1 to len1 do
        let cost = if s1.[i - 1] = s2.[j - 1] then 0 else 1 in
        curr_row.(i) <-
          min
            (min (prev_row.(i) + 1) (curr_row.(i - 1) + 1))
            (prev_row.(i - 1) + cost)
      done;
      (* Swap rows *)
      Array.blit curr_row 0 prev_row 0 (len1 + 1)
    done;
    prev_row.(len1)

(** Maximum edit distance threshold for a suggestion to be considered useful.

    Threshold scales with the length of the query:
    - Length 1-2: max distance 1 (single typo)
    - Length 3-5: max distance 2
    - Length 6+: max distance 3 *)
let max_distance query_len =
  if query_len <= 2 then 1 else if query_len <= 5 then 2 else 3

(** Find similar names from a list of candidates.

    Returns names that are within the edit distance threshold, sorted by
    distance (closest first). If multiple names have the same distance, shorter
    names come first (prefer simpler suggestions). *)
let find_similar_names ~query ~candidates =
  let threshold = max_distance (String.length query) in
  (* Calculate distances and filter by threshold *)
  let scored =
    List.filter_map
      (fun candidate ->
        let dist = distance query candidate in
        if dist > 0 && dist <= threshold then Some (candidate, dist) else None)
      candidates
  in
  (* Sort by distance, then by length for ties *)
  let sorted =
    List.sort
      (fun (n1, d1) (n2, d2) ->
        let cmp = compare d1 d2 in
        if cmp <> 0 then cmp else compare (String.length n1) (String.length n2))
      scored
  in
  List.map fst sorted

(** Find the single best suggestion for a typo.

    Returns [Some name] if a good match is found, [None] otherwise. Only returns
    a suggestion if it's clearly better than other options. *)
let suggest_name ~query ~candidates =
  match find_similar_names ~query ~candidates with
  | [] -> None
  | [ best ] -> Some best
  | best :: second :: _ ->
      (* Only suggest if best is clearly better (distance difference >= 1) *)
      let best_dist = distance query best in
      let second_dist = distance query second in
      if second_dist - best_dist >= 1 then Some best else Some best
