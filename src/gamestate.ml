open GameData

type resource = {
  id: resource_type;
  amount: int;
}


type tile_type = Diamonds | Rock | Grass
type person = string
type coordinates = int * int

type building = {
  building_type: building_type;
  coordinates: coordinates;
  workers: person list;
  residents: person list;
}

type building_option = Some of building | None

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

let rec check_coor_tiles x y = function
  | h::t -> if (fst h.coordinates) = x && (snd h.coordinates) = y then h.name 
            else check_coor_tiles x y t
  | [] -> Grass

let rec check_coor_building x y (building_lst:building list) = 
  match building_lst with 
    | h::t -> if (fst h.coordinates) = x && (snd h.coordinates) = y then (Some h.building_type)
              else check_coor_building x y t
    | [] -> None


let get_tile_at (coor:coordinates) state =
  check_coor_tiles (fst coor) (snd coor) state.tiles


let step state = failwith "todo"

let can_place_building building_type coor state = 
  failwith "todo"

let place_building building_type coor worker_amt state = 
  failwith "todo"

let set_workers coor amt state = 
  failwith "todo"

let alive state = 
  failwith "todo"

let get_buildings state =
  failwith "todo"

let get_building_at coor state =
  let b = check_coor_building (fst coor) (snd coor) state.buildings in 
  match b with 
  | Some building -> building 
  | None -> 

let get_user_resources state =
  failwith "todo"



let get_bounds state =
  failwith "todo"