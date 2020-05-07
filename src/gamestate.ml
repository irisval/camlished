open GameData
open Yojson.Basic.Util
open Printf

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

let json_tile j = {
  name  = j |> member "tile type" |> to_string |> GameData.tile_type_of_string;
  coordinates = 
  (j |> member "x coordinate" |>  to_int, j |> member "y coordinate" |>  to_int )
}

let user_data j = {
  turn_id = j |> member "turn id" |> to_int;
  resources = j |> member "resources" |> to_list |> List.map json_resource;
  buildings = j |> member "buildings" |> to_list |> List.map json_building;
  tiles = j |> member "tiles" |> to_list |> List.map json_tile;
  game_data =
    try
      "src/sampleGameData.json" |> Yojson.Basic.from_file |> GameData.from_json
    with
    | Yojson.Json_error _ -> failwith "Can't find file"

}

let from_json j = try user_data j
  with Type_error (s, _) -> failwith ("Parsing error: " ^ s)


let resources_to_json (rsc_lst: resource list) : string = 
  let convert = function
    | {id=i; amount=a} -> String.concat "" ["{\"name\":\""; i; "\", \"amount\":";
                        (a |> string_of_int);"}"]
    | _ -> failwith "Malformed State Data." in 
  let strs : string list = List.map convert rsc_lst in 
  String.concat "," strs

let people_to_json (people : person list) : string = 
  let (p_str : string list) = 
    List.map (fun p -> String.concat "" ["\""; p; "\""]) people in 
  String.concat "," p_str

let buildings_to_json (bg_lst : building list) : string = 
  let convert = function
    | {building_type=b; coordinates = c; workers = w; residents = r} -> 
      String.concat "" ["{\"building type\":\""; b; "\", \"x coordinate\":";
        (fst c |> string_of_int); ", \"y coordinate\":"; (snd c |> string_of_int);
        ", \"workers\": ["; (people_to_json w); "], \"residents\": ["; 
        (people_to_json r); "]}"]
    | _ -> failwith "Malformed State Data." in 
  let strs : string list = List.map convert bg_lst in 
  String.concat "," strs

let tiles_to_json (tile_lst : tile list) : string = 
  let convert = function 
  | {name=n; coordinates=c} -> 
    String.concat "" ["{\"tile type\": \""; (GameData.string_of_tile_type n); 
      "\", \"x coordinate\": "; (fst c |> string_of_int);  ", \"y coordinate\": "; 
      (snd c |> string_of_int); "}"]
    | _ -> failwith "Malformed State Data." in 
  let strs : string list = List.map convert tile_lst in 
  String.concat "," strs

let save (st:t) = 
  match st with 
  | {turn_id=t; resources=rscs; buildings=bgs; tiles=tiles;_} -> 
    let t_str = "\"turn id\": " ^ (t |> string_of_int) in 
    let r_str = String.concat "" ["\"resources\":["; resources_to_json rscs; "]"] in
    let b_str = String.concat "" ["\"buildings\":["; buildings_to_json bgs; "]"] in
    let ti_str = String.concat "" ["\"tiles\":["; tiles_to_json tiles; "]"] in
    let body = String.concat "," [t_str; r_str; b_str; ti_str] in 
    let oc = open_out "src/sampleSavedState.json" in    
    fprintf oc "%s\n" ( String.concat "" ["{"; body; "}"] );
    close_out oc


(** [building_resource_generation resources bt st] updates the state with
    the resources generated by a building *)
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

let turns st =
  st.turn_id

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

let assign_workers_b b amt st =
  let unassigned = unassigned_workers st in
  if (List.length b.workers) < (max_workers b.building_type st.game_data)
      && amt <= List.length unassigned then
    let updated_workers = add_workers b.workers amt unassigned in
    let b' = {b with workers = updated_workers} in
    {st with buildings =
          b'::(List.filter (fun x -> x <> b) st.buildings)}
  else
    raise IllegalWorkerAssignment

let assign_workers_c coor amt st =
  match get_building_at coor st with
  | Some b -> assign_workers_b b amt st
  | None -> raise IllegalWorkerAssignment

(** [remaining_workers new_len acc worker_list] is [acc]
    with [new_len] workers from [worker_list]? *)
let rec remaining_workers new_len acc (worker_lst: person list) : (person list) =
  match worker_lst with
  | h::t -> if List.length acc = new_len then acc
    else remaining_workers new_len (h::acc) t
  | [] -> acc

let unassign_workers_b b amt st =
  if amt <= 0 then raise IllegalWorkerAssignment else
  let num_workers = List.length b.workers in
  let new_num_workers = (if amt > num_workers then raise IllegalWorkerAssignment else num_workers - amt) in
  let workers' = remaining_workers new_num_workers [] b.workers in
  let b' = {b with workers = workers'} in
  {st with buildings =
        b'::(List.filter (fun x -> x <> b) st.buildings)}

let unassign_workers_c coor amt st =
  match get_building_at coor st with
  | Some b -> unassign_workers_b b amt st
  | None -> raise IllegalWorkerAssignment

let building_residents b =
  b.residents

let building_workers b =
  b.workers

let alive st = (population st = 0) |> not

(*
Save data file after stepping for testing purposes
let step st = 
let s = {turn_id = st.turn_id + 1;
               resources = step_buildings st;
               buildings = st.buildings;
               tiles = st.tiles;
               game_data = st.game_data} in
  save s;
  st *)

let step st = 
let s = {turn_id = st.turn_id + 1;
               resources = step_buildings st;
               buildings = st.buildings;
               tiles = st.tiles;
               game_data = st.game_data} in
  save s;
  st

let get_bounds st = GameData.get_bounds st.game_data

let get_test_building =
  {building_type= "tent"; coordinates=(5,6);workers=[];residents=["john doe"; "jane doe"]}

let get_test_tile = GameData.Water

let get_test_placed_building =
  {building_type= "silo"; coordinates=(0,0);workers=[];residents=[]}
