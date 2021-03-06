(**
    Input handler for Camlished
 *)

(** Type of an building placement state. *)
type placing_state = PickLocation | AssignWorkers of int
(** Type of a worker assignment or unassignment state. *)
type adjust_workers_state = Assign | Unassign

(** Type of an input action. *)
type action = 
  | Observing
  | Placing of (placing_state * GameData.building_type * GameState.coordinates)
  | BuildingPicker of int
  | Inspecting of GameState.coordinates
  | AdjustWorkers of (adjust_workers_state * GameState.coordinates * int)
  | QuitConfirm
  | Quit

(** Type of an input state. *)
type t = {
  msg : string;
  act : action;
}

(** Represents input command types that modify input state.
    Input commands are mapped to one or more keys on keyboard.*)
type command = 
  | New
  | Load
  | Up
  | Down
  | Left
  | Right
  | Select
  | Cancel
  | Step
  | PlaceBuilding
  | Inspect
  | Assign
  | Unassign
  | Save
  | Quit
  | Unrecognized


(**[starting] is the initial input state upon starting the game. *)
val starting : t

(**[controls_text t] gives help text for controls associated with [t].*)
val controls_text : t -> string

(**[receive_command c t gs] is [(t',gs')] where [t'] and [gs'] are
   [t] and [gs] after receiving [c].*)
val receive_command : command -> t -> GameState.t -> (t * GameState.t)