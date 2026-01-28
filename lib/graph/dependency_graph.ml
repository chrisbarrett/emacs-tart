(** Module dependency graph for incremental re-checking.

    Maintains forward and reverse indexes for efficient queries in both
    directions: "what does X depend on" and "what depends on X". *)

(** {1 Types} *)

type module_id = string
type edge_kind = Require | Autoload | Open | Include | Sibling
type edge = { target : module_id; kind : edge_kind }

type t = {
  forward : (module_id, edge list) Hashtbl.t;
  reverse : (module_id, module_id list) Hashtbl.t;
}

(** {1 Construction} *)

let create () : t = { forward = Hashtbl.create 64; reverse = Hashtbl.create 64 }

(** {1 Internal Helpers} *)

(** Add a reverse edge: target -> source *)
let add_reverse_edge (g : t) ~(source : module_id) ~(target : module_id) : unit
    =
  let deps =
    match Hashtbl.find_opt g.reverse target with
    | Some deps -> deps
    | None -> []
  in
  if not (List.mem source deps) then
    Hashtbl.replace g.reverse target (source :: deps)

(** Remove a reverse edge: target -> source *)
let remove_reverse_edge (g : t) ~(source : module_id) ~(target : module_id) :
    unit =
  match Hashtbl.find_opt g.reverse target with
  | Some deps ->
      let deps' = List.filter (fun d -> d <> source) deps in
      if deps' = [] then Hashtbl.remove g.reverse target
      else Hashtbl.replace g.reverse target deps'
  | None -> ()

(** {1 Edge Management} *)

let add_edges (g : t) (module_id : module_id) (edges : edge list) : unit =
  (* Remove old reverse edges first *)
  (match Hashtbl.find_opt g.forward module_id with
  | Some old_edges ->
      List.iter
        (fun e -> remove_reverse_edge g ~source:module_id ~target:e.target)
        old_edges
  | None -> ());

  (* Add new forward edges *)
  Hashtbl.replace g.forward module_id edges;

  (* Add new reverse edges *)
  List.iter
    (fun e -> add_reverse_edge g ~source:module_id ~target:e.target)
    edges

let remove_module (g : t) (module_id : module_id) : unit =
  (* Remove forward edges and their reverse counterparts *)
  (match Hashtbl.find_opt g.forward module_id with
  | Some edges ->
      List.iter
        (fun e -> remove_reverse_edge g ~source:module_id ~target:e.target)
        edges;
      Hashtbl.remove g.forward module_id
  | None -> ());

  (* Remove reverse edges pointing to this module *)
  match Hashtbl.find_opt g.reverse module_id with
  | Some dependents ->
      List.iter
        (fun dep ->
          match Hashtbl.find_opt g.forward dep with
          | Some edges ->
              let edges' = List.filter (fun e -> e.target <> module_id) edges in
              if edges' = [] then Hashtbl.remove g.forward dep
              else Hashtbl.replace g.forward dep edges'
          | None -> ())
        dependents;
      Hashtbl.remove g.reverse module_id
  | None -> ()

let get_edges (g : t) (module_id : module_id) : edge list =
  match Hashtbl.find_opt g.forward module_id with
  | Some edges -> edges
  | None -> []

(** {1 Dependency Queries} *)

let direct_dependents (g : t) (module_id : module_id) : module_id list =
  match Hashtbl.find_opt g.reverse module_id with
  | Some deps -> deps
  | None -> []

let dependents (g : t) (module_id : module_id) : module_id list =
  (* BFS to collect all transitive dependents *)
  let visited = Hashtbl.create 16 in
  let result = ref [] in
  let queue = Queue.create () in

  (* Start with direct dependents *)
  List.iter (fun dep -> Queue.add dep queue) (direct_dependents g module_id);

  while not (Queue.is_empty queue) do
    let dep = Queue.pop queue in
    if not (Hashtbl.mem visited dep) then (
      Hashtbl.add visited dep ();
      result := dep :: !result;
      List.iter (fun d -> Queue.add d queue) (direct_dependents g dep))
  done;

  List.rev !result

let direct_dependencies (g : t) (module_id : module_id) : module_id list =
  List.map (fun e -> e.target) (get_edges g module_id)

let dependencies (g : t) (module_id : module_id) : module_id list =
  (* BFS to collect all transitive dependencies *)
  let visited = Hashtbl.create 16 in
  let result = ref [] in
  let queue = Queue.create () in

  (* Start with direct dependencies *)
  List.iter (fun dep -> Queue.add dep queue) (direct_dependencies g module_id);

  while not (Queue.is_empty queue) do
    let dep = Queue.pop queue in
    if not (Hashtbl.mem visited dep) then (
      Hashtbl.add visited dep ();
      result := dep :: !result;
      List.iter (fun d -> Queue.add d queue) (direct_dependencies g dep))
  done;

  List.rev !result

(** {1 Cycle Detection} *)

type cycle = module_id list

let detect_cycles (g : t) : cycle list =
  (* Tarjan's algorithm variant for cycle detection *)
  let visited = Hashtbl.create 16 in
  let rec_stack = Hashtbl.create 16 in
  let cycles = ref [] in

  let rec dfs (path : module_id list) (module_id : module_id) : unit =
    if Hashtbl.mem rec_stack module_id then (
      (* Found a cycle - extract it from the path.
         Path is [most_recent; ...; module_id; ...], we want [module_id; ...; most_recent] *)
      let rec extract_cycle acc = function
        | [] -> acc
        | m :: rest ->
            if m = module_id then m :: acc else extract_cycle (m :: acc) rest
      in
      let cycle = extract_cycle [] path in
      if List.length cycle > 0 then
        (* Normalize cycle to start from lexicographically smallest element *)
        let min_idx =
          let min_elem = List.fold_left min (List.hd cycle) (List.tl cycle) in
          let rec find_idx i = function
            | [] -> 0
            | m :: rest -> if m = min_elem then i else find_idx (i + 1) rest
          in
          find_idx 0 cycle
        in
        let len = List.length cycle in
        let normalized =
          List.init len (fun i -> List.nth cycle ((i + min_idx) mod len))
        in
        (* Only add if not already detected (same cycle from different starting points) *)
        if not (List.mem normalized !cycles) then
          cycles := normalized :: !cycles)
    else if not (Hashtbl.mem visited module_id) then (
      Hashtbl.add visited module_id ();
      Hashtbl.add rec_stack module_id ();
      List.iter
        (fun dep -> dfs (module_id :: path) dep)
        (direct_dependencies g module_id);
      Hashtbl.remove rec_stack module_id)
  in

  (* Run DFS from all modules *)
  Hashtbl.iter (fun module_id _ -> dfs [] module_id) g.forward;

  !cycles

(** {1 Inspection} *)

let modules (g : t) : module_id list =
  (* Collect all module IDs that have edges *)
  let all = Hashtbl.create 32 in
  Hashtbl.iter (fun m _ -> Hashtbl.replace all m ()) g.forward;
  Hashtbl.iter (fun m _ -> Hashtbl.replace all m ()) g.reverse;
  Hashtbl.fold (fun m () acc -> m :: acc) all []

let mem (g : t) (module_id : module_id) : bool =
  Hashtbl.mem g.forward module_id || Hashtbl.mem g.reverse module_id

let is_empty (g : t) : bool =
  Hashtbl.length g.forward = 0 && Hashtbl.length g.reverse = 0
