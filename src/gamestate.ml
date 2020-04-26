open GameData
open Yojson.Basic.Util

exception OccupiedTile
exception IllegalWorkerAssignment
exception IllegalResourceCollection

(* define types  *)
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
  game_data: GameData.t;
}

type t = game_state

(* read from json *)
let json_resource j = {
  id = j |> member "name" |> to_string;
  amount = j |> member "amount" |> to_int;
}

let json_person j =
  j |> to_string

let json_building j = {
  building_type = j |> member "building type" |> to_string;
  coordinates = (j |> member "x coordinate" |> to_int, j |> member "y coordinate" |> to_int);
  workers = j |> member "workers" |> to_list |> List.map json_person;
  residents = j |> member "residents" |> to_list |> List.map json_person
}

let coor_json (xy_lst:int list) = (List.hd xy_lst, List.nth xy_lst 1)

let json_tile tile_type coor_lst : (tile list) =
  List.fold_left 
    (fun acc coor -> {name=tile_type; coordinates=coor_json coor}::acc) [] coor_lst

let json_tile_type j : (tile list) = 
  let tt t : tile_type = (t |> member "tile type" |> to_string |> tile_type_of_string) in
  let coor t = (t |> member "coordinates" |> convert_each (fun lst -> lst |> convert_each (fun x -> x |> to_int))) in 
  List.fold_left (fun acc tile_j -> (json_tile (tt tile_j) (coor tile_j)) @ acc) [] j

let user_data j = {
  turn_id = j |> member "turn id" |> to_int;
  resources = j |> member "resources" |> to_list |> List.map json_resource;
  buildings = j |> member "buildings" |> to_list |> List.map json_building;
  tiles = j |> member "tiles" |> to_list |> json_tile_type;
  game_data =
    try 
      "src/sampleGameData.json" |> Yojson.Basic.from_file |> GameData.from_json
    with
    | Yojson.Json_error _ -> failwith "can't find file"

}

let from_json j = try user_data j 
  with Type_error (s, _) -> failwith ("Parsing error: " ^ s)

(* gamestate methods *)
(* let rec update_resource (rsc_lst:resource list) (id:resource_type) amt acc = 
   match rsc_lst with 
   | {id=i;amount=a}::t -> if i = id then update_resource t id amt ({id=i;amount=a+amt}::acc)
    else update_resource t id amt ({id=i;amount=a}::acc)
   | [] -> acc

   let building_resource_generation resources (bt:GameData.building_type) st : (resource list)= 
   let gen_lst = GameData.active_generation bt st.game_data in
   let rsc_lst = resources in 
   List.fold_left (fun acc (f:active_generation) -> (update_resource acc f.resource f.output [])) rsc_lst gen_lst *)

let building_resource_generation resources (bt:GameData.building_type) st : (resource list) = 
  let gen_list = GameData.active_generation bt st.game_data in
  let update r : resource =
    let a = r.amount in 
    match List.find_opt (fun (g:active_generation) -> g.resource = r.id) gen_list with
    | Some {resource=r; output=o} -> {id=r; amount= a + o}
    | None -> r
  in
  List.map update resources


(** [step_buildings buildings resources] is [resources] after stepping
    all buildings using [resources] *)
let step_buildings st : resource list =
  let brg resources building = building_resource_generation resources building.building_type st in
  List.fold_left brg st.resources st.buildings

let get_user_resources st =
  List.map (fun r -> (r.id, r.amount)) st.resources

let get_buildings st =
  st.buildings

let get_tiles st =
  st.tiles

let get_game_data st =
  st.game_data

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

let tile_rep_at coor st =
  match get_tile_at coor st with
  | Some n -> n
  | None -> Grass

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
     tiles = st.tiles;
     game_data = st.game_data}

let can_place_building building_type coor st = 
  is_empty coor st

(* let gather_resource coor st = 
   match get_building_type_at coor st with 
   | Some b -> 
   | None -> raise IllegalResourceCollection *)

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

let assign_workers coor amt st = 
  match get_building_at coor st with
  | Some b ->
    let unassigned = unassigned_workers st in
    if (List.length b.workers) < (max_workers b.building_type st.game_data)
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


let unassign_workers coor amt st =
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

let alive st = (population st = 0) |> not

let step st = {turn_id = st.turn_id + 1;
               resources = step_buildings st;
               buildings = st.buildings;
               tiles = st.tiles;
               game_data = st.game_data}

let get_bounds st = GameData.get_bounds st.game_data

let get_test_building = 
  {building_type= "Tent"; coordinates=(5,6);workers=[];residents=["John Doe"]}

let get_test_tile = GameData.Water

let get_test_placed_building = 
  {building_type= "Silo"; coordinates=(7,8);workers=[];residents=[]}
