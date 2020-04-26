type input = Input.t
type game = Gamestate.t

(** [draw input game] clears the screen and draws the game with [game] 
    and the input state [input].*)
val draw : input -> game -> unit
