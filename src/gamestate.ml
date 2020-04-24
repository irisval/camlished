open GameData

exception OccupiedTile
exception IllegalWorkerAssignment

type resource = {
  id: resource_type;
  amount: int;
}

type person = string
type coordinates = int * int

type building = {
  building_type: building_type;
  coordinates: coordinates;
  workers: person list;
  residents: person list;
}

type tile = {
  name: tile_type;
  coordinates: coordinates;
}

type game_state = {
  turn_id: int;
  resources: resource list;
  buildings: building list;
  tiles: tile list;
}

type t = game_state

let init_state = {
  turn_id = 0;
  resources = [];
  buildings = [];
  tiles = [];
}

let step st = {turn_id = st.turn_id + 1; resources = st.resources; buildings = st.buildings; tiles = st.tiles}

let get_user_resources st =
  st.resources

let get_buildings st =
  st.buildings

let get_building_at coor st : (building option) = 
  List.find_opt (fun (b:building) -> b.coordinates = coor) st.buildings

let get_building_type_at coor st = 
  match get_building_at coor st with 
  | Some b -> Some b.building_type
  | None -> None

let get_tile_at coor st = 
  let rec find_coor x y tiles : (tile_type option) =
    match tiles with 
    | {name=n; coordinates=c;}::t -> 
      if (fst c) = x && (snd c) = y then Some n else find_coor x y t
    | [] -> None

  in find_coor (fst coor) (snd coor) st.tiles


let is_empty coor st =
  match get_building_at coor st with 
  | Some _ -> false 
  | None -> match get_tile_at coor st with
    | Some _ -> false 
    | None -> true

(** [make_building building_type coor] returns a building of type
    [building_type] at [coor] *)
let make_building building_type coor = 
  {building_type = building_type; coordinates = coor; workers = []; residents = []} 

let place_building building_type coor st = 
  if is_empty coor st |> not then raise OccupiedTile
  else let b = make_building building_type coor in 
    {turn_id = st.turn_id;
     resources = st.resources;
     buildings = b::st.buildings;
     tiles = st.tiles }

let population st =
  List.fold_left (fun acc b -> acc + List.length b.residents) 0 st.buildings

(** [list_diff l1 l2] is [l1] removing the elements of [l2] *)
let list_diff l1 l2 =
  List.filter (fun x -> not (List.mem x l2)) l1

let all_residents st =
  List.fold_left (fun acc b -> acc @ b.residents) [] st.buildings

let assigned_workers st =
  List.fold_left (fun acc b -> acc @ b.workers) [] st.buildings

let unassigned_workers st =
  list_diff (all_residents st) (assigned_workers st)

(** [add_workers acc amt workers] adds [amt] from [workers] to [acc] *)
let rec add_workers acc amt workers =
  if amt = 0 then acc
  else match workers with
    | [] -> raise IllegalWorkerAssignment
    | h::t -> add_workers (h::acc) (amt - 1) t

let assign_workers coor amt dt st = 
  match get_building_at coor st with
  | Some b ->
    let unassigned = unassigned_workers st in
    if (List.length b.workers) < (max_workers b.building_type dt)
    && amt <= List.length unassigned then
      let updated_workers = add_workers [] amt unassigned in
      let b' = {b with workers = updated_workers} in
      {st with buildings =
                 b'::(List.filter (fun x -> x <> b) st.buildings)}
    else
      raise IllegalWorkerAssignment
  | None -> raise IllegalWorkerAssignment

let rec remaining_workers new_len acc (worker_lst: person list) : (person list) = 
  match worker_lst with 
  | h::t -> if List.length acc = new_len then acc
    else remaining_workers new_len (h::acc) t
  | [] -> acc


let unassign_workers coor amt st=
  if amt <= 0 then st
  else
    match get_building_at coor st with
    | Some b -> let num_workers = List.length b.workers in 
      let new_num_workers = (if amt > num_workers then num_workers else num_workers - amt) in
      let workers' = remaining_workers new_num_workers [] b.workers in 
      let b' = {b with workers = workers'} in
      {st with buildings =
                 b'::(List.filter (fun x -> x <> b) st.buildings)}
    | None -> raise IllegalWorkerAssignment

let alive st = 
  (population st = 0) |> not

(* let get_bounds = GameData.bounds *)
