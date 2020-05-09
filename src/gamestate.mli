(** The type representing coordinates *)
type coordinates = (int*int)

(** The type representing a resource *)
type resource

(** The type representing a person *)
type person = string

(**  The type representing a building*)
type building
(** The type representing a tile *)
type tile

(** The type representing a game state *)
type t

(** Raised in an attempt to place a building on an occupied tile. *)
exception OccupiedTile

(** Raised in an attempt to perform an operation on a non defined resource *)
exception IllegalResourceType

(** Raised in failure to collect a resource *)
exception IllegalResourceCollection

(** Raised in an attempt to assign a worker to a nonexistent building. *)
exception IllegalWorkerAssignment 


(** [from_json j] is the user data that [j] represents.
    Requires: [j] is a valid JSON game state representation. *)
val from_json : Yojson.Basic.t -> t


(** [save st] saves the game by writing the game state to the data file. *)
val save : t -> unit


(** [st u_rsc] updates the resource list in [st] with the resource type and 
    amount associated with [u_rsc]. Creates new resource if [u_rsc] not found. 
    Reqruies: u_rsc is a valid resource type *)
val update_rsc : resource -> t -> t

(** [get_rsc_amt st rsc] gives the amount of the resource [rsc] stores in [st]. *)
val get_rsc_amt : resource -> t -> int

(** [building_consumption_gen st bt] updates the state with the resources 
    consumed and generated by [bt] *)
val building_consumption_gen : GameData.building_type -> t -> t

(** [building_active_gen st bt] updates the state with the resources actively
    generated by [bt] *)
val building_active_gen : GameData.building_type -> t -> t

(** [step st] steps one turn through the game state [st]. 
    TODO: incorporate game changes in between steps *)
val step : t -> t

(** [turns st] is the number of turns in [st] *)
val turns : t -> int

(* [get_buildings st] gives a list of all the buildings on the state's map. *)
val get_buildings : t -> building list

(* [get_tiles st] gives a list of all the tiles on the state's map. *)
val get_tiles : t -> tile list

(* [get_game_data st] gives the game dta. *)
val get_game_data : t -> GameData.t

(** [get_user_resources st] is the list of resources the user has *)
val get_user_resources : t -> (string * int) list

(* [get_building_at coor st] returns the building option of the building located
   at the coordinates [coor] on the state map. Returns None if there is no building. *)
val get_building_at : coordinates -> t -> building option

(* [get_building_type_at coor st] returns the building_type option of the building located
   at the coordinates [coor] on the state map. Returns None if there is no building. *)
val get_building_type_at : coordinates -> t -> GameData.building_type option

(* [get_tile_at coor st] returns the tile_type option of the tile
   at the coordinates [coor] on the state map. Returns None if tile is empty. *)
val get_tile_at : coordinates -> t -> GameData.tile_type option

(* [tile_rep_at coor st] returns the tile_type of the tile
   at the coordinates [coor] on the state map. Returns Grass if tile is empty. *)
val tile_rep_at : coordinates -> t -> GameData.tile_type

(** [is_empty coor st] gives whether or not a tile at the coordinates [coor]
    is empty (it has no buildings and no explicit tile?) *)
val is_empty : coordinates -> t -> bool

(** [place_building building_type coor worker_amt st] places a building of the
    type [building_type] on the state map at the coordinates [coor]. *)
val place_building : GameData.building_type -> coordinates  -> t -> t

(** [can_place_building building_type coor st] gives whether a building of 
    type [building_type] can be placed on [coordinates]. *)
val can_place_building : GameData.building_type -> coordinates  -> t -> bool

(** [gather_resource coor st] collects a resource generated by the building at
    [coor]. Raises IllegalResourceCollection if there is no building or resource
    t be collected. *)
(* val gather_resource : coordinates -> t -> t *)

(** [population st] gives the numerical population of [st]. *)
val population : t -> int

(** [all_residents st] is the list of all people who live in [st] *)
val all_residents : t -> person list

(** [assigned_workers st] is the list of all people with job who live in [st] *)
val assigned_workers : t -> person list

(** [unassigned_workers st] is the list of people who don't have job in [st] *)
val unassigned_workers : t -> person list

(** [assign_workers_b b amt st] assigns [amt] workers to [b],
    raising IllegalWorkerAssignment if not enough unassigned workers. *)
val assign_workers_b : building -> int -> t -> t

(** [assign_workers_c coor amt st] assigns [amt] workers to the building at
    [coor] if possible, else raises IllegalWorkerAssignment. *)
val assign_workers_c : coordinates -> int -> t -> t

(** [unassign_workers_b b amt st] unassigns [amt] workers or the max possible
    from [b], raising IllegalWorkerAssignment if trying to unassign negative. *)
val unassign_workers_b : building -> int -> t -> t

(** [unassign_workers_c coor amt st] unassigns [amt] workers from building at
    [coor] if possible, else raises IllegalWorkerAssignment. *)
val unassign_workers_c : coordinates -> int -> t -> t

(** [building_residents b] is the list of residents at [b] *)
val building_residents : building -> person list

(** [building_workers b] is the list of workers at [b] *)
val building_workers : building -> person list

(** [max_population st] is the sum of max residents of each building in [st] *)
val max_population : t -> int

(** [cap_list_length n l] is [l] with at most [n] elements, from front *)
val cap_list_length : int -> 'a list -> 'a list

(** [remove_ppl p l] is [l] randomly removing people with probability [p] *)
val remove_ppl : float -> person list -> person list

(** [kill_residents b st] is [b] after [kill_some] with death rate from [st] *)
val kill_residents : building -> t -> building

(** [clean_dead_workers b living] is [b] after removing workers that are no
    longer in the [living] *)
val clean_dead_workers : building -> person list -> building

(** [step_deaths bl st] is [bl] after culling and cleaning each building.
    [st] is only used for its game data -> death chance *)
val step_deaths : building list -> t -> building list

(** [new_residents p l] is the list of babies possibly born to people in [l]
    with probability [p] each *)
val new_residents : float -> person list -> person list

(** [alive st] is if there are still living people in [st] *)
val alive : t -> bool

(** [get_bounds] returns the bounds of the game *)
val get_bounds : t -> GameData.bounds

(** [get_test_building] generates a building for testing *)
val get_test_building : building 

(** [get_test_tile] generates a [tile_type] for testing *)
val get_test_tile : GameData.tile_type

(** [get_test_placed_building] generates a building for placement testing *)
val get_test_placed_building : building

(** [get_test_food] generates a food resource for resource testing *)
val get_test_food : resource

(** [get_test_planks] generates a plank resource for resource testing *)
val get_test_planks : resource

(** [get_test_wood] generates a wood resource for resource testing *)
val get_test_wood : resource

(** [get_test_metal] generates a metal resource for resource testing *)
val get_test_metal : resource

(** [get_test_ore] generates a ore resource for resource testing *)
val get_test_ore : resource
