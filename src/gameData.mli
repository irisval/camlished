(**
    Data of the game
*)

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

val max_residents : 

(* val get_building_types : t -> (building_type_id*GameData.building_type) list *)