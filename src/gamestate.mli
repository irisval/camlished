open GameData

type coordinates = (int*int)
type resource

type building
type tile_type = Diamonds | Rock
type tile
type game_state

(** The type representing a game state *)
type t

val init_state : t

val step : t -> t

val can_place_building : GameData.building_type -> coordinates -> t -> bool

val place_building : GameData.building_type -> coordinates -> int -> t -> t

val set_workers : coordinates -> int -> t -> t

val alive : t -> bool

val get_buildings : t -> building list

val get_building_at : coordinates -> t -> building option

(** [get_user_resources st] is the list of resources the user has *)
val get_user_resources : t -> resource list

val get_tile_at : coordinates -> t -> tile_type

val get_bounds : t -> coordinates
