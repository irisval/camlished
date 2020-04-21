type coordinates
type building_type_id
type building_id
type building_type
type building
type tile_type

(** The type representing a game state *)
type t

val init_state : t

val step : t -> t

val can_place_building : building_type_id -> coordinates -> t -> bool

val place_building : building_type_id -> coordinates -> int -> t -> t

val set_workers : building_id -> int -> t -> t

val alive : t -> bool

val get_resources : t -> (string*int) list

val get_building_types : t -> (building_type_id*building_type) list

val get_buildings : t -> building list

val get_building_at : coordinates -> t -> building option

val get_resource_at : t -> tile_type

val get_bounds : t -> (int*int)
