(** The type representing coordinates *)
type coordinates = (int*int)

(** The type representing a resource *)
type resource

(** The type representing a perons *)
type person

(**  The type representing a building*)
type building

(** The type representing a tile *)
type tile

(** The type representing a game state *)
type t

(** Raised in an attempt to place a building on an occupied tile. *)
exception OccupiedTile

(** Raised in an attempt to assign a worker to a nonexistent building. *)
exception IllegalWorkerAssignment 


(** [from_json j] is the user data game state that [j] represents.
    Requires: [j] is a valid JSON adventure representation. *)
val from_json : Yojson.Basic.t -> t

(* [init_state] gives the initial state of the game when the turn is 0. *)
val init_state : t

(** [step st] steps one turn through the game state [st]. 
    TODO: incorporate game changes in between steps *)
val step : t -> t

(* [get_buildings st] gives a list of all the buildings on the state's map. *)
val get_buildings : t -> building list

(** [get_user_resources st] is the list of resources the user has *)
val get_user_resources : t -> resource list

(* [get_building_at coor st] returns the building option of the building located
   at the coordinates [coor] on the state map. Returns None if there is no building. *)
val get_building_at : coordinates -> t -> building option

(* [get_building_at coor st] returns the building_type option of the building located
   at the coordinates [coor] on the state map. Returns None if there is no building. *)
val get_building_type_at : coordinates -> t -> GameData.building_type option

(* [get_tile_at coor st] returns the tile_type option of the tile
   at the coordinates [coor] on the state map. Returns None if tile is empty. *)
val get_tile_at : coordinates -> t -> GameData.tile_type option

(** [is_empty coor st] gives whether or not a tile at the coordinates [coor]
    is empty *)
val is_empty : coordinates -> t -> bool

(** [place_building building_type coor worker_amt st] places a building of the
    type [building_type] on the state map at the coordinates [coor]. *)
val place_building : GameData.building_type -> coordinates  -> t -> t

(** [population st] gives the numerical population of [st]. *)
val population : t -> int

(** [all_residents st] is the list of all people who live in [st] *)
val all_residents : t -> int

(** [assigned_workers st] is the list of all people with job who live in [st] *)
val assigned_workers : t -> int

(** [unassigned_workers st] is the list of people who don't have job in [st] *)
val unassigned_workers : t -> person list

(** [assign_workers coor amt dt st] assigns [amt] workers to the building at
    [coor] if possible, else raises IllegalWorkerAssignment. *)
val assign_workers : coordinates -> int -> GameData.t -> t -> t

(** [unassign_workers coor amt dt st] unassigns [amt] workers from building at
    [coor] if possible, else raises IllegalWorkerAssignment. *)
val unassign_workers : coordinates -> int -> GameData.t -> t -> t

(** [alive st] gives where the population of a society has died*)
val alive : t -> bool

(** [get_bounds] returns the bounds of the game*)
val get_bounds :  GameData.bounds
