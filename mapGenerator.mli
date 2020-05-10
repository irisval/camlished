(**
    Map generator
 *)

(** Map generator tiles *)
type tile = {
  name: GameData.tile_type;
  coordinates: (int*int);
}

(** [generate (width, height)] generates map *)
 val generate : (int * int) -> tile list