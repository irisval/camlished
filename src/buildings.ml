type resources
type coordinates = int*int

module BuildingType = struct
  type t = {
    name: string;
    cost : resources;
    output : resources;
  }
end

type building_type_id = int

module Building = struct
  type t = {
    type_id: building_type_id;
    position: coordinates;
    worker_count: int;
  }
end