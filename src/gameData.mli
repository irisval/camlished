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

(** The type of seasons *)
type season = Summer | Fall | Winter | Spring

(** The type of tiles *)
type tile_type = Grass | Mountain | Water | Forest

(** The type of building placement rules *)
type placement_rule_type = On | Next

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

(** [tile_type_of_string s] is the tile variant representation of s *)
val tile_type_of_string : string -> tile_type

(** [string_of_tile_type t] the string representation of the tile variant t *)
val string_of_tile_type : tile_type -> string

(** [placement_rule_of_string s] is the placement rule type variant
    representation of s *)
val placement_rule_of_string : string -> placement_rule_type

(** [building_types dt] is the list of building types in [dt] *)
val building_types : t -> building_type list

(** [resource_types dt] is the list of resource types in [d] *)
val resource_types : t -> resource_type list

(** [birth_rate dt] is the natural birth rate in [dt] *)
val birth_rate : t -> float

(** [death_rate dt] is the natural death rate in [dt] *)
val death_rate : t -> float

(** [death_rate_starving dt] is the death rate when 0 food in [dt] *)
val death_rate_starving : t -> float

(** [death_rate_winter dt] is the death rate during winter in [dt] *)
val death_rate_winter : t -> float

(** [warmth bt dt] is the warmth of building type [bt] in [dt]. 0 indicates full
    susceptibility to death rate winter, 1 uses regular death rate *)
val warmth : building_type -> t -> float

(** [max_residents bt dt] is the maximum number of residents
    that can live in building [bt] in game data [dt] *)
val max_residents : building_type -> t -> int

(** [max_workers bt dt] is the maximum number of workers
    that can work in building [bt] in game data [dt] *)
val max_workers : building_type -> t -> int

(** [min_req_workers bt dt] is the minimum number of workers
    needed for building [bt] in game data [dt] to generate resources. *)
val min_req_workers  : building_type -> t -> int

(** [rsc_requirements bt dt] is the list of resource requirements, which are
    the resource types and amounts needed to build [bt] in game data [dt] *)
val rsc_requirements : building_type -> t -> requirement list

(** [placement_requirements bt dt] is the list of placement requirements, which are
    the rules needed to build [bt] in game data [dt] *)
val placement_requirements : building_type -> t -> placement_rule list

(** [active_generation bt dt] gives the active generation details of a building *)
val active_generation : building_type -> t -> active_generation list

(** [max_rsc_output bt dt] gives the max resource output of building type [bt] *)
val max_rsc_output : building_type -> t -> int

(** [consumption_generation bt dt] gives the consumption generation details
    of a building *)
val consumption_generation : building_type -> t -> consumption_generation list

(** [storage bt dt] is the storage increase(s) that building [bt]
    provides in game data [dt] *)
val storage : building_type -> t -> storage list

(** [get_bounds] returns the bounds of the game*)
val get_bounds : t -> bounds
