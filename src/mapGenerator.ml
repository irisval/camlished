open Perlin

type tile = {
  name: GameData.tile_type;
  coordinates: (int*int);
}

let gen_tile x y off_x off_y =
  let u =  (float_of_int x) +. off_x in
  let v = (float_of_int y) +. off_y in
  let otherwise alt x = match x with None -> alt | x -> x in
  let gen_river = let river_p = perlin (u*.0.03) (v*.0.06) in 
    if abs_float (river_p-.0.5) < 0.019 then Some GameData.Water else None in 
  let gen_mountain = 
    let mountain_p = perlin (u*.0.2) (v*.0.4) in
    if mountain_p > 0.6 then Some GameData.Mountain else None in
  let gen_forest =
    (* let forest_p = perlin (u*.0.1 +. 128.2) (v*.0.2 +. 128.2) in *)
    let forest_p = perlin (u*.0.05 +. 128.2) (v*.0.1 +. 128.2) in
    if forest_p > 0.6 then Some GameData.Forest else None  in
  match gen_river |> otherwise gen_mountain |> otherwise gen_forest with
  | Some t -> Some {
      name = t;
      coordinates = (x,y)
    }
  | None -> None


let generate (width,height) =
  Random.self_init ();
  let off_x = Random.float 255. in
  let off_y = Random.float 255. in
  let rec add_row tiles y = 
    let rec aux tiles x = if x = -1 then tiles 
      else let g = gen_tile x y off_x off_y in
        match g with
        | Some t -> t :: (aux tiles (x-1))
        | None -> (aux tiles (x-1))
    in
    if y = -1 then tiles else
      add_row (aux tiles (width-1)) (y-1)
  in
  add_row [] (height-1)


