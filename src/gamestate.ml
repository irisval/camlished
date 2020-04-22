open GameData

type resource = {
  id: resource_type;
  amount: int;
}

type tile_type
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
  coordinates: coordinates list;
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
  failwith "todo"

let get_user_resources state =
  failwith "todo"

let get_tile_at coor state =
  failwith "todo"

let get_bounds state =
  failwith "todo"