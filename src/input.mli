type game = Gamestate.t
type action = 
  | Observing
  | Placing of (Gamestate.building_type * Gamestate.coordinates)
  | BuildingPicker of int
type t = {
  msg : string;
  act : action;
}

(** Represents input command types that modify input state.
    Input commands are mapped to one or more keys on keyboard.*)
type command = 
  | Up
  | Down
  | Left
  | Right
  | Select
  | Cancel
  | Step
  | PlaceBuilding
  | Unrecognized

(**[starting] is the initial input state upon starting the game. *)
val starting : t

(**[controls_text t] gives help text for controls associated with [t].*)
val controls_text : t -> string

(**[receive_command c t gs] is [(t',gs')] where [t'] and [gs'] are
   [t] and [gs] after receiving [c].*)
val receive_command : command -> t -> game -> (t*game)