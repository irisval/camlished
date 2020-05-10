open Yojson.Basic.Util

(* define types  *)
type resource_type = string
type building_type = string
type bounds = int * int

type season = Summer | Fall | Winter | Spring
type tile_type = Grass | Mountain | Water | Forest
type placement_rule_type = On | Next


type requirement = {
  resource: resource_type;
  amount: int;
}

type placement_rule = {
  rule_type: placement_rule_type;
  tile: tile_type;
}

type consumption_generation = {
  input_resource: resource_type;
  input_amount: int;
  output_resource: resource_type;
  output_amount: int;
}

type active_generation = {
  resource: resource_type;
  output: int;
}

type storage = {
  resource: resource_type;
  capacity: int;
}


type building_properties = {
  name: building_type;
  warmth: float;
  max_residents: int;
  max_workers: int;
  min_req_workers: int;
  requirements: requirement list;
  placement_rules: placement_rule list;
  max_rsc_output: int;
  consumption_generation: consumption_generation list;
  active_generation: active_generation list;
  storages: storage list;
}

type game_data = {
  resource_types: resource_type list;
  building_properties: building_properties list;
  bounds: int * int;
  birth_rate: float;
  death_rate: float;
  death_rate_starving: float;
  death_rate_winter: float;
}

type t = game_data

let tile_type_of_string = function
  | "grass" -> Grass
  | "mountain" -> Mountain
  | "water" -> Water
  | "forest" -> Forest
  | _ -> failwith "Invalid string to tile type conversion"

let string_of_tile_type = function
  | Grass -> "grass" 
  | Mountain -> "mountain"
  | Water -> "water"
  | Forest -> "forest"
  | _ -> failwith "Invalid string to tile type conversion"

let placement_rule_of_string = function
  | "on" -> On
  | "next" -> Next
  | _ -> failwith "Invalid string to placement rule conversion"

(* read from json *)
(** Todo: Add json specs *)
let json_resource j = j |> to_string

let json_requirements j = {
  resource = j |> member "resource" |> to_string;
  amount = j |> member "amount" |> to_int;
}

let json_placement j = {
  rule_type = j |> member "type" |> to_string |> placement_rule_of_string;
  tile = j |> member "tile" |> to_string |> tile_type_of_string;
}

let json_consumption_gen j = {
  input_resource = j |> member "input resource" |> to_string;
  input_amount = j |> member "input amount" |> to_int;
  output_resource = j |> member "output resource" |> to_string;
  output_amount = j |> member "output amount" |> to_int;
}

let json_active_gen j = {
  resource = j |> member "resource" |> to_string;
  output = j |> member "output" |> to_int;
}

let json_storage j = {
  resource = j |> member "resource" |> to_string;
  capacity = j |> member "capacity" |> to_int;
}

let json_building j = {
  name = j |> member "name" |> to_string;
  warmth = j |> member "warmth" |> to_float;
  max_residents = j |> member "max residents" |> to_int;
  max_workers = j |> member "max workers" |> to_int;
  min_req_workers = j |> member "min req workers" |> to_int;
  requirements = j |> member "requirements" |> to_list |> List.map json_requirements;
  placement_rules = j |> member "placement rule" |> to_list |> List.map json_placement;
  max_rsc_output = j |> member "max rsc output" |> to_int;
  consumption_generation = j |> member "consumption generation" |> to_list |> List.map json_consumption_gen;
  active_generation = j |> member "active generation" |> to_list |> List.map json_active_gen;
  storages = j |> member "storage" |> to_list |> List.map json_storage;
}

let game_data j = {
  resource_types = j |> member "resource types" |> to_list |> List.map json_resource;
  building_properties = j |> member "building properties" |> to_list |> List.map json_building;
  bounds = (j |> member "width" |> to_int, j |> member "height" |> to_int);
  birth_rate = j |> member "birth rate" |> to_float;
  death_rate = j |> member "death rate" |> to_float;
  death_rate_starving = j |> member "death rate starving" |> to_float;
  death_rate_winter = j |> member "death rate winter" |> to_float;
}

let from_json j = try game_data j 
  with Type_error (s, _) -> failwith ("Parsing error: " ^ s)

(* gamedata methods *)
let building_types dt =
  List.map (fun b -> b.name) dt.building_properties

let resource_types dt =
  dt.resource_types

let birth_rate dt =
  dt.birth_rate

let death_rate dt =
  dt.death_rate

let death_rate_starving dt =
  dt.death_rate_starving

let death_rate_winter dt =
  dt.death_rate_winter

(* gamestate methods *)
(** [properties b dt] are the building properties for [b] in [dt] *)
let properties bt dt : building_properties =
  List.find (fun bp -> bp.name = bt) dt.building_properties

let get_bounds dt = 
  dt.bounds

let warmth bt dt =
  (properties bt dt).warmth

let max_residents bt dt =
  (properties bt dt).max_residents

let max_workers bt dt =
  (properties bt dt).max_workers

let min_req_workers bt dt =
  (properties bt dt).min_req_workers

let rsc_requirements bt dt =
  (properties bt dt).requirements

let placement_requirements bt dt =
  (properties bt dt).placement_rules

let active_generation bt dt = 
  (properties bt dt).active_generation

let max_rsc_output bt dt =
  (properties bt dt).max_rsc_output

let consumption_generation bt dt =
  (properties bt dt).consumption_generation

let storage bt dt =
  (properties bt dt).storages
