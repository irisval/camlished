(**
    Data of the game
*)

type t

(** The type of resources *)
type resource_type = string

(** The type of buildings *)
type building_type = string

(** The placement cost *)
type placement_cost = {
  resource: resource_type;
  cost: int;
}

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
  active_generation: unit;
  storages: storage list;
}

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
val active_generation : building_type -> t -> unit

(** [storage b dt] is the storage increase(s) that building [b]
    provides in game data [dt] *)
val storage : building_type -> t -> storage list

(* val get_building_types : t -> (building_type_id*GameData.building_type) list *)