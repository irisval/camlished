type resource_type = string
type building_type = string

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

type t = {
  resource_types: resource_type list;
  building_properties: building_properties list;
}

(** [properties b dt] are the building properties for [b] in [dt] *)
let properties b dt : building_properties =
  List.find (fun bp -> bp.name = b) dt

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