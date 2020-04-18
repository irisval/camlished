type t

type id

val get_all : t -> (string*int) list

val get : id -> t -> int

val set : id -> int -> t -> t