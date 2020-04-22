type input_state
type game_state = Gamestate.t

(** [draw input game] clears the screen and draws the game with [game] 
    and the input state [input].*)
val draw : input_state -> game_state -> unit


(* TODO: temp *)
val temp_input_state : input_state