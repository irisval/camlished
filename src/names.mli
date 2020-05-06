(** Name generator **)

(** [init x] sets the random seed to [x] *)
val init : int -> unit

(** [rand_partname ()] is the next single name *)
val rand_partname : unit -> string

(** [rand_fullname ()] is the next full name *)
val rand_fullname : unit -> string

