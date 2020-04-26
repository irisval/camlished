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
type tile_type = Grass | Rock | Water | Trees

(** Active Generation Details *)
type active_generation = {
  resource: resource_type;
  output: int;
}

(** The placement cost *)
type placement_cost = {
  resource: resource_type;
  cost: int;
}

(** [from_json j] is the user data game that [j] represents.
    Requires: [j] is a valid JSON game data representation. *)
val from_json : Yojson.Basic.t -> t

(** The extra storage that a building can provide *)
type storage = {
  resource: resource_type;
  capacity: int;
}

(** The type representing properties tied to a building type *)
type building_properties = {
  name: building_type;
  max_residents: int;
  max_workers: int;
  placement_costs: placement_cost list;
  active_generation: active_generation list;
  storages: storage list;
}

(** [tile_type_of_string s] is the tile variant representation of s *)
val tile_type_of_string : string -> tile_type

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

(** [placement_cost b dt] is the list of placement costs, which are
    the resource types and amounts, to build [b] in game data [dt] *)
val placement_cost : building_type -> t -> placement_cost list

(** [active_generation b dt] is something with active generation *)
val active_generation : building_type -> t -> active_generation list

(** [storage b dt] is the storage increase(s) that building [b]
    provides in game data [dt] *)
val storage : building_type -> t -> storage list

(** [get_bounds] returns the bounds of the game*)
val get_bounds : t -> bounds