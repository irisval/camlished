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