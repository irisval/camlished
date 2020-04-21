type resources
type coordinates = int*int

type building_type = {
  name: string;
  cost : resources;
  output : resources;
}

type building_type_id = int

(* module Building = struct
   type t = {
    type_id: building_type_id;
    position: coordinates;
    worker_count: int;
   }
   end *)

type building = {
  type_id: building_type_id;
  position: coordinates;
  worker_count: int;
}