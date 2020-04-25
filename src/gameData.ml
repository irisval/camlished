open Yojson.Basic.Util

(* define types  *)
type resource_type = string
type building_type = string
type bounds = int * int

type tile_type = Grass | Rock | Water | Trees

type placement_cost = {
  resource: resource_type;
  cost: int;
}

type storage = {
  resource: resource_type;
  capacity: int;
}

type building_properties = {
  name: building_type;
  max_residents: int;
  max_workers: int;
  placement_costs: placement_cost list;
  active_generation: unit;
  storages: storage list;
}

type game_data = {
  resource_types: resource_type list;
  building_properties: building_properties list;
  bounds: int * int;
}

type t = game_data

let tile_type_of_string = function
  | "grass" -> Grass
  | "rock" -> Rock
  | "water" -> Water
  | "trees" -> Trees
  | _ -> failwith "Invalid string to tile type conversion"

(* read from json *)
let json_resource j = j |> to_string

let json_placement j = {
  resource = j |> member "resource" |> to_string;
  cost = j |> member "cost" |> to_int;
}

let json_storage j = {
  resource = j |> member "resource" |> to_string;
  capacity = j |> member "capacity" |> to_int;
}

let json_building j = {
  name = j |> member "name" |> to_string;
  max_workers = j |> member "max workers" |> to_int;
  max_residents = j |> member "max residents" |> to_int;
  placement_costs = j |> member "placement cost" |> to_list |> List.map json_placement;
  active_generation = ();
  storages = j |> member "storage" |> to_list |> List.map json_storage;
}

let game_data j = {
  resource_types = j |> member "resource types" |> to_list |> List.map json_resource;
  building_properties = j |> member "building properties" |> to_list |> List.map json_building;
  bounds = (j |> member "width" |> to_int, j |> member "height" |> to_int);
}

let from_json j = try game_data j 
  with Type_error (s, _) -> failwith ("Parsing error: " ^ s)

(* gamedata methods *)
let building_types dt =
  List.map (fun b -> b.name) dt.building_properties

let resource_types dt =
  dt.resource_types

(* gamestate methods *)
(** [properties b dt] are the building properties for [b] in [dt] *)
let properties b dt : building_properties =
  List.find (fun bp -> bp.name = b) dt.building_properties

let get_bounds dt = 
  dt.bounds

let max_residents b dt =
  (properties b dt).max_residents

let max_workers b dt =
  (properties b dt).max_workers

let placement_cost b dt =
  (properties b dt).placement_costs

let active_generation b dt =
  ignore b; ignore dt; failwith "Unimplemented"

let storage b dt =
  (properties b dt).storages