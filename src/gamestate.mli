type coordinates = (int*int)
(* TODO: TEMP *)
type building_type = string 
type building_type_id
type building_id
type building
type tile_type = Grass | Rock

type t

val init_state : t

val step : t -> t

val can_place_building : building_type -> coordinates -> t -> bool

val place_building : building_type -> coordinates -> t -> t

val set_workers : building_id -> t -> t

val alive : t -> bool

val get_resources : t -> (string*int) list

val get_buildings : t -> building


val get_building_at : coordinates -> t -> building option

val get_resource_at : coordinates -> t -> tile_type

val get_bounds : t -> (int*int)




(* NEW BELOW *)

val get_building_types : t -> building_type list

val get_building_type_name : building_type -> string