open GameData
open Yojson.Basic.Util
open Printf


exception IllegalWorkerAssignment
exception IllegalResourceType
exception IllegalResourceCollection

(* ====== Block: Define types ====== *)
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
  world_name: string;
  turn_id: int;
  resources: resource list;
  buildings: building list;
  tiles: tile list;
  game_data: GameData.t;
}

type t = game_state

(* ====== Block: Messaging system ====== *)
let messages = ref []

let clear_log () =
  messages := []

let log s =
  messages := s::(!messages)

let read_log () =
  !messages

(* ====== Block Json operations ====== *)
(** [json_resource j] constructs a single resource from [j] *)
let json_resource j = {
  id = j |> member "name" |> to_string;
  amount = j |> member "amount" |> to_int;
}

(** [json_person j] constructs a single person from [j] *)
let json_person j =
  j |> to_string

(** [json_building j] constructs a single building from [j] *)
let json_building j = {
  building_type = j |> member "building type" |> to_string;
  coordinates = (j |> member "x coordinate" |> to_int, 
                 j |> member "y coordinate" |> to_int);
  workers = j |> member "workers" |> to_list |> List.map json_person;
  residents = j |> member "residents" |> to_list |> List.map json_person
}

(** [json_tile j] constructs a tile from [j] *)
let json_tile j = {
  name  = j |> member "tile type" |> to_string |> GameData.tile_type_of_string;
  coordinates = 
    (j |> member "x coordinate" |> to_int,
     j |> member "y coordinate" |>  to_int)
}

(** [user_data j] constructs a gamestate from [j] *)
let user_data j = {
  world_name = j |> member "world name" |> to_string |> String.trim;
  turn_id = j |> member "turn id" |> to_int;
  resources = j |> member "resources" |> to_list |> List.map json_resource;
  buildings = j |> member "buildings" |> to_list |> List.map json_building;
  tiles = j |> member "tiles" |> to_list |> List.map json_tile;
  game_data =
    try
      "src/sampleGameData.json" |> Yojson.Basic.from_file |> GameData.from_json
    with
    | Yojson.Json_error _ -> failwith "Error parsing game data file."
}

(** [from_json j] turns a user game data file into a gamestate *)
let from_json j = try user_data j
  with Type_error (s, _) -> failwith ("Parsing error: " ^ s)

(** [resources_to_json rsc_lst] gives the json string format of the resources
    in [rsc_lst] *)
let resources_to_json (rsc_lst: resource list) : string = 
  let convert = function
    | {id=i; amount=a} -> String.concat ""
                            ["{\"name\":\""; i; "\", \"amount\":";
                             (a |> string_of_int);"}"]
    | _ -> failwith "Malformed State Data." in 
  let strs : string list = List.map convert rsc_lst in 
  String.concat "," strs

(** [people_to_json people] gives the json string format of the pepole
    in [people] *)
let people_to_json (people : person list) : string = 
  let (p_str : string list) = 
    List.map (fun p -> String.concat "" ["\""; p; "\""]) people in 
  String.concat "," p_str

(** [buildings_to_json bg_lst] gives the json string format of the buildings
    in [bg_lst] *)
let buildings_to_json (bg_lst : building list) : string = 
  let convert = function
    | {building_type=b; coordinates = c; workers = w; residents = r} -> 
      String.concat "" ["{\"building type\":\""; b; "\", \"x coordinate\":";
                        (fst c |> string_of_int); ", \"y coordinate\":"; 
                        (snd c |> string_of_int); ", \"workers\": ["; 
                        (people_to_json w); "], \"residents\": ["; 
                        (people_to_json r); "]}"]
    | _ -> failwith "Malformed State Data." in 
  let strs : string list = List.map convert bg_lst in 
  String.concat "," strs

(** [tiles_to_json tile_lst] gives the json string format of the tiles
    in [tile_lst] *)
let tiles_to_json (tile_lst : tile list) : string = 
  let convert = function 
    | {name=n; coordinates=c} -> 
      String.concat "" ["{\"tile type\": \""; (GameData.string_of_tile_type n); 
                        "\", \"x coordinate\": "; (fst c |> string_of_int);
                       ", \"y coordinate\": "; 
                        (snd c |> string_of_int); "}"]
    | _ -> failwith "Malformed State Data." in 
  let strs : string list = List.map convert tile_lst in 
  String.concat "," strs

(** [save] saves the current gamestate [st] to a data file*)
let save (st:t) = 
  match st with 
  | {world_name=w;
    turn_id=t; resources=rscs; buildings=bgs; tiles=tiles;_} -> 
    let n_str = "\"world name\": \"" ^ w ^ "\"" in
    let t_str =
      "\"turn id\": " ^ (t |> string_of_int) in 
    let r_str =
      String.concat "" ["\"resources\":["; resources_to_json rscs; "]"] in
    let b_str =
      String.concat "" ["\"buildings\":["; buildings_to_json bgs; "]"] in
    let ti_str = String.concat "" ["\"tiles\":["; tiles_to_json tiles; "]"] in
    let body = String.concat "," [n_str; t_str; r_str; b_str; ti_str] in 
    let oc = open_out ("src/" ^  (Str.global_replace (Str.regexp " ") "" w)
                      ^ ".json") in    
    fprintf oc "%s\n" ( String.concat "" ["{"; body; "}"] );
    close_out oc

(* ====== Block: State operations ====== *)
let get_random  gd : coordinates =
  let x = Random.int (fst (GameData.get_bounds gd)) in 
  let y = Random.int (snd (GameData.get_bounds gd)) in 
  (x, y)

let set_grass tile_lst coor_lst : tile list = 
  List.filter (fun t -> not (List.mem t.coordinates coor_lst)) tile_lst

let initial_tents coor1 coor2 : building list =
  [
    {
      building_type = "tent";
      coordinates = coor1;
      workers = [];
      residents = ["Bryan"; "Changyuan"]
    };
    {
      building_type = "tent";
      coordinates = coor2;
      workers = [];
      residents = ["Easter"; "Bunny"]
    }
  ]

let initial_state (name:string) =
  let game_data= 
    try
      "src/sampleGameData.json" |> Yojson.Basic.from_file |> GameData.from_json
    with
    | Yojson.Json_error _ -> failwith "Error parsing game data file." in
  let tl = MapGenerator.generate (GameData.get_bounds game_data)
           |> List.map (fun (e:MapGenerator.tile) -> {
                 name = e.name;
                 coordinates = e.coordinates
               }) in
  let t1 = get_random game_data in 
  let t2 = get_random game_data in 
  {world_name=name; turn_id=0; resources= [{id="food"; amount=10}]; 
   buildings= initial_tents t1 t2;
   tiles=set_grass tl [t1; t2]; game_data = game_data}

let turns st =
  st.turn_id

let year st =
  st.turn_id / 360

let season st =
  match st.turn_id / 90 mod 4 with
  | 0 -> Summer
  | 1 -> Fall
  | 2 -> Winter
  | 3 -> Spring
  | _ -> failwith "Invalid season"

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
    | Some Water -> false
    | _ -> true

(* ====== Block: Building and resource operations ====== *)

(** [make_building building_type coor] returns a building of type
    [building_type] at [coor] *)
let make_building building_type coor =
  {building_type = building_type; coordinates = coor; 
   workers = []; residents = []}

let get_rsc_amt rt st : int =
  match List.find_opt (fun r -> r.id = rt) st.resources with 
  | Some rsc -> rsc.amount
  | None -> 0

let meets_rsc_reqs bt st = 
  let req_lst = GameData.rsc_requirements bt st.game_data in
  let under = List.filter (fun (r:requirement) -> 
      (get_rsc_amt r.resource st) <= r.amount) req_lst in 
  List.length under = 0

let meets_placement_reqs bt coor st =
  let req_lst = GameData.placement_requirements bt st.game_data in 
  let under = List.filter (fun (p:placement_rule) -> 
      let t = p.tile in
      match p.rule_type with 
      | On ->  tile_rep_at coor st <> t
      | Next -> match coor with (x,y) ->
        (tile_rep_at (x-1, y) st <> t) && (tile_rep_at (x+1,y) st <> t)
        && (tile_rep_at (x,y-1) st <> t) && (tile_rep_at (x,y+1) st <> t)
    ) req_lst in 
  List.length under = 0

let is_valid_bt bt st = 
  let b = 
    List.find_opt (fun b -> b = bt) (GameData.building_types st.game_data) in 
  match b with 
  | Some _ -> true
  | None -> false

let can_place_building_at bt coor st =
  is_valid_bt bt st && is_empty coor st 
  && meets_placement_reqs bt coor st && meets_rsc_reqs bt st

(** [rsc_check st rsc] checks that [rsc] is defined in the resource types of the 
    game data in [st] *)
let rsc_check rsc st =
  let rsc_lst = st.game_data |> GameData.resource_types in 
  match List.find_opt (fun r -> r = rsc.id) rsc_lst with 
  | Some _ -> rsc 
  | None -> raise IllegalResourceType

let update_rsc (u_rsc : resource) st : t =
  let u_rsc' = rsc_check u_rsc st in 
  let rec update (rsc_lst:resource list) (acc:resource list)  = 
    match rsc_lst with 
    | r::t -> if r.id = u_rsc'.id 
              then {id=r.id; amount = r.amount + u_rsc'.amount}::t@acc
              else update t (r::acc)
    | [] -> {id=u_rsc'.id; amount = u_rsc'.amount}::acc in 
  {st with resources = update st.resources []}

(** [pay_building_cost bt st] gives an updated resource list
    with the resource cost required to place building with type [bt] in [st] *)
let pay_building_cost bt (st:t) : resource list  = 
  let updated = List.fold_left (fun st' (req:requirement) ->
      update_rsc {id=req.resource; amount=req.amount * -1} st'
    ) st (rsc_requirements bt st.game_data) in 
  updated.resources

let place_building bt coor st =
  let b = make_building bt coor in
  { st with
    resources = pay_building_cost bt st;
    buildings = b::st.buildings }

(* ====== Block: State population/worker assignment ====== *)

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
  if amt <= List.length unassigned then
    let updated_workers = add_workers b.workers amt unassigned in
    let b' = {b with workers = updated_workers} in
    {st with buildings =
               b'::(List.filter (fun x -> x <> b) st.buildings)}
  else raise IllegalWorkerAssignment

let assign_workers_c coor amt st =
  match get_building_at coor st with
  | Some b -> assign_workers_b b amt st
  | None -> raise IllegalWorkerAssignment

(** [remaining_workers new_len acc worker_list] is [acc]
    with [new_len] workers from [worker_list] *)
let rec remaining_workers new_len acc worker_lst : (person list) =
  match worker_lst with
  | h::t -> if List.length acc = new_len then acc
    else remaining_workers new_len (h::acc) t
  | [] -> acc

let unassign_workers_b b amt st =
  if amt <= 0 then raise IllegalWorkerAssignment else
    let num_workers = List.length b.workers in
    let new_num_workers = 
      if amt > num_workers then raise IllegalWorkerAssignment 
      else num_workers - amt in
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

(** [worker_rsc_output bt base_output st] gives the amount of resource that 
    would be produced (assuming that input reqs are met) depending on the worker
    properties of [bt], the amount of workers in [st], and the [base_output] of
    the resource being produced *)
let worker_rsc_output (b:building) base_output st = 
  let bt = b.building_type in 
  let num_workers = List.length (building_workers b) in 
  let min_workers = min_req_workers bt st.game_data in
  let max_workers =  max_workers bt st.game_data in

  if num_workers < min_workers then 0
  else let r =  float_of_int (num_workers - min_workers) /. 
                float_of_int (max_workers - min_workers) in 
    let a =  Float.log10 ((9.0 *. r) +. 1.0) *. 
             float_of_int (max_rsc_output bt st.game_data)  in
    if (int_of_float a) + base_output > max_rsc_output bt st.game_data
    then max_rsc_output bt st.game_data
    else ((int_of_float a) + base_output)

(** [sufficient_workers b st] gives whether [b] has enough buildings to 
    produce resources *)
let sufficient_workers (b:building) st = 
  let num_workers = List.length (building_workers b) in 
  let min_workers = min_req_workers b.building_type st.game_data in
  num_workers >= min_workers

let building_consumption_gen (b:building) st = 
  let gen_lst = GameData.consumption_generation b.building_type st.game_data in
  List.fold_left (fun st' g ->
      if (get_rsc_amt g.input_resource st') >= g.input_amount &&
         sufficient_workers b st then
        let input_st' = 
          (update_rsc {id=g.input_resource; amount=g.input_amount * -1} st') in 
        let new_amt = worker_rsc_output b g.output_amount st in 
        (update_rsc {id=g.output_resource; amount= new_amt} input_st')
      else st')
    st gen_lst

let building_active_gen (b:building) st =
  let gen_lst = GameData.active_generation b.building_type st.game_data in
  List.fold_left (fun st' (g:active_generation) -> 
      let new_amt = worker_rsc_output b g.output st in 
      update_rsc {id=g.resource; amount=new_amt} st') st gen_lst 

let active_gen_rsc_amt (b:building) st = 
  let gen_lst = GameData.active_generation b.building_type st.game_data in
  List.fold_left (fun a (g:active_generation) -> 
      let new_amt = worker_rsc_output b g.output st in 
      {id=g.resource; amount=new_amt}::a
    ) [] gen_lst

let con_gen_rsc_amt (b:building) st = 
  let gen_lst = GameData.consumption_generation b.building_type st.game_data in
  List.fold_left (fun a g ->
      if (get_rsc_amt g.input_resource st) >= g.input_amount &&
         sufficient_workers b st then
        let input_req = {id=g.input_resource; amount=g.input_amount * -1} in 
        let new_amt = worker_rsc_output b g.output_amount st in 
        let output_req = {id=g.output_resource; amount= new_amt}  in 
        (input_req, output_req)::a
      else a)
    [] gen_lst

(** [step_buildings buildings resources] is the state with updated resources
    after stepping through all of the buildings and executing their resource
    generation. *)
let step_buildings st =
  List.fold_left (fun st' b -> 
      let active_st' = building_active_gen b st' in
      building_consumption_gen b active_st') st st.buildings

(* life & death *)
let max_population st =
  List.fold_left (fun acc b ->
      acc + (max_residents b.building_type st.game_data)) 0 st.buildings

let rec cap_list_length n l =
  match l with
  | [] -> l
  | h::t ->
    if n <= 0 then []
    else h::(cap_list_length (n - 1) t)

let living_residents bl : person list =
  List.fold_left (fun acc b -> acc @ b.residents) [] bl

let remove_ppl p l : person list =
  List.filter (fun n -> (fun boo -> if not boo then log (n ^ " died"); boo)
                  (Random.float 1.0 > p)) l

let death_chance food warmth season st =
  let normal = death_rate st.game_data in
  let starve = death_rate_starving st.game_data in
  let winter_raw = death_rate_winter st.game_data in
  let winter = winter_raw -. (winter_raw -. normal) *. warmth in
  match season with
  | Winter when food <= 0 -> max winter starve
  | Winter -> max winter normal
  | _ when food <= 0 -> max starve normal
  | _ -> normal

let kill_residents b st : building =
  let death_chance = death_chance (get_rsc_amt "food" st)
      (warmth b.building_type st.game_data) (season st) st in
  let residents' = remove_ppl death_chance b.residents in
  {b with residents = residents'}

let clean_dead_workers b living : building =
  let is_alive w = List.mem w living in
  {b with workers = List.filter is_alive b.workers}

let step_deaths bl st : building list =
  let kr = (fun b -> kill_residents b st) in
  let b_culled = List.map kr bl in
  let living = living_residents b_culled in
  let cdw = (fun b -> clean_dead_workers b living) in
  List.map cdw b_culled

let new_residents p l : person list =
  let maybe_birth = (fun acc _ ->
      if Random.float 1.0 > p then acc
      else (Names.rand_fullname ())::acc) in
  List.fold_left maybe_birth [] l

let baby_to_building name b st : (bool * building) =
  if List.length b.residents < max_residents b.building_type st.game_data then
    (log (name ^ " was born");
     (true, {b with residents = name::b.residents}))
  else
    (false, b)

let dist_babies babies st b_arr : building array =
  let rec dist_babies_rec babies : unit =
    match babies with
    | [] -> ()
    | h::t ->
      let rand_i = Random.int (Array.length b_arr) in
      match baby_to_building h b_arr.(rand_i) st with
      | (false, _) ->
        dist_babies_rec babies (* try again *)
      | (true, b) ->
        b_arr.(rand_i) <- b;
        dist_babies_rec t
  in dist_babies_rec babies; b_arr

let step_births bl st : building list =
  let babies = (fun b -> new_residents (birth_rate st.game_data) b.residents) in
  let all_baby = List.map babies bl |> List.flatten in
  let baby_capped = cap_list_length
      ((max_population st) - (living_residents bl |> List.length)) all_baby in
  let b_arr = Array.of_list bl in
  dist_babies baby_capped st b_arr |> Array.to_list

let eat_food st : t =
  let consumed food = {
    food with
    amount =
      let new_amt = food.amount - (population st) in
      if new_amt < 0 then (log "\nPopulation starving!"; 0) else new_amt
  } in
  let resources' = List.map (fun r ->
      if r.id = "food" then consumed r else r) st.resources
  in {st with resources = resources'}

(** [step_population st] is [st] after stepping for pop *)
let step_population st : t =
  let st_after_eating = eat_food st in
  let bl_after_deaths = step_deaths st.buildings st_after_eating in
  let bl_after_births = step_births bl_after_deaths st_after_eating in
  {st_after_eating with buildings = bl_after_births}

let alive st = (population st = 0) |> not

let step st =
  clear_log ();
  { (step_buildings st |> step_population)
    with turn_id = st.turn_id + 1 }

(* ====== Block: Test functions ====== *)
let get_bounds st = GameData.get_bounds st.game_data

let get_test_building =
  {building_type= "tent";
    coordinates=(5,6);
    workers=[];
    residents=["john doe"; "jane doe"]}

let get_test_tile = GameData.Water

let get_test_placed_buildings =
  [{building_type= "tent"; coordinates=(0,0);workers=[];residents=[]};
   {building_type= "quarry"; coordinates=(2,2);workers=[];residents=[]};
   {building_type= "quarry"; coordinates=(3,3);workers=[];residents=[]};
   {building_type= "fishing dock"; coordinates=(9,9);workers=[];residents=[]}]

let get_test_food = {id="food";amount=2}
let get_test_planks = {id="planks";amount=3}
let get_test_wood = {id="wood";amount=3}
let get_test_metal = {id="metal";amount=0}
let get_test_ore = {id="ore";amount=0}
let get_test_stone = {id="stone";amount=3}

let get_test_food_type = "food"
let get_test_planks_type = "planks"
let get_test_wood_type = "wood"
let get_test_metal_type = "metal"
let get_test_ore_type = "ore"
let get_test_stone_type = "stone"



