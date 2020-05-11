(**
    Renderer for Camlished
 *)

(** [draw input game] clears the screen and draws the game with [game] 
    and the input state [input].*)
val draw : Input.t -> GameState.t -> unit

(** [you_died input game] clears the screen and gives a message when the
    player dies.*)
val you_died : Input.t -> GameState.t -> unit

(** [full_clear ()] clears the screen.*)
val full_clear : unit -> unit