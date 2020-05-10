type input = Input.t
type game = GameState.t

(** [draw input game] clears the screen and draws the game with [game] 
    and the input state [input].*)
val draw : input -> game -> unit

(** [you_died input game] clears the screen and gives a message when the
    player dies.*)
val you_died : input -> game -> unit