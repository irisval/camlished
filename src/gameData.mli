(**
    Data of the game
*)

type t

(** The type of resources *)
type resource_type = string

(** The type of buildings *)
type building_type = string

(** The width and height of the map *)
type bounds = int * int

(** The type of tiles *)
type tile_type = Grass | Mountain | Water | Forest | Flowers | Sand

(** The type of building placement rules *)
type placement_rule_type = On | Next | Only

(** The placement cost of a building *)
type requirement = {
  resource: resource_type;
  amount: int;
}

(** The rule for placing a building onto a location *)
type placement_rule = {
  rule_type: placement_rule_type;
  tile: tile_type;
}

(** The details for resources that require an input to be generated*)
type consumption_generation = {
  input_resource: resource_type;
  input_amount: int;
  output_resource: resource_type;
  output_amount: int;
}

(** The details for resources that can be generated without an input *)
type active_generation = {
  resource: resource_type;
  output: int;
}

(** The extra storage that a building can provide *)
type storage = {
  resource: resource_type;
  capacity: int;
}

(** [from_json j] is the user data game that [j] represents.
    Requires: [j] is a valid JSON game data representation. *)
val from_json : Yojson.Basic.t -> t

(** The type representing properties tied to a building type *)
type building_properties = {
  name: building_type;
  max_residents: int;
  max_workers: int;
  requirements: requirement list;
  placement_rule: placement_rule list;
  consumption_generation: consumption_generation list;
  active_generation: active_generation list;
  storages: storage list;
}

(** [tile_type_of_string s] is the tile variant representation of s *)
val tile_type_of_string : string -> tile_type

(** [placement_rule_of_string s] is the placement rule type variant 
representation of s *)
val placement_rule_of_string : string -> placement_rule_type

(** [building_types dt] is the list of building types in [dt] *)
val building_types : t -> building_type list

(** [resource_types dt] is the list of resource types in [d] *)
val resource_types : t -> resource_type list

(** [max_residents b dt] is the maximum number of residents
    that can live in building [b] in game data [dt] *)
val max_residents : building_type -> t -> int

(** [max_workers b dt] is the maximum number of workers
    that can work in building [b] in game data [dt] *)
val max_workers : building_type -> t -> int

(** [placement_requirements b dt] is the list of placement costs, which are
    the resource types and amounts, to build [b] in game data [dt] *)
val placement_requirements : building_type -> t -> requirement list

(** [active_generation b dt] is something with active generation *)
val active_generation : building_type -> t -> active_generation list

(** [storage b dt] is the storage increase(s) that building [b]
    provides in game data [dt] *)
val storage : building_type -> t -> storage list

(** [get_bounds] returns the bounds of the game*)
val get_bounds : t -> bounds