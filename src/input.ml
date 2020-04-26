type game = Gamestate.t
type action = 
  | Observing
  | Placing of (GameData.building_type * Gamestate.coordinates)
  | BuildingPicker of int

type t = {
  msg : string;
  act : action;
}

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

let starting = {
  msg = "Welcome to Camlished!";
  act = Observing
}

let controls_text t = match t.act with
  | Observing -> "Space: advance time."
  | _ -> ""

let bad_command_text = "Unrecognized command."

let center_of_map gs = match Gamestate.get_bounds gs with
  | (x,y) -> (x/2, y/2)

let receive_observing c t gs =
  let gs' = match c with
    | Step -> Gamestate.step gs
    | _ -> gs in
  let t' = {
    t with act = match c with
      | PlaceBuilding -> BuildingPicker 0
      | _ -> Observing
  } in (t',gs')

let in_bounds (width,height) (x,y) =
  (x >= 0) && (x < width) && (y >= 0) && (y < height)

let receive_placing c btype (x,y) t gs =
  let pos' = match c with
    | Up -> (x,y-1)
    | Down -> (x,y+1)
    | Left -> (x-1,y)
    | Right -> (x+1,y)
    | _ -> (x,y)
  in
  let can_place = Gamestate.can_place_building btype (x,y) gs in
  let gs' = match c with
    | Select ->
      if can_place then Gamestate.place_building btype (x,y) gs else gs
    | _ -> gs in 
  let t' = {
    t with act = match c with
      | Cancel -> Observing
      | Select -> if can_place then Observing else t.act
      | Up | Down | Left | Right ->
        if in_bounds (Gamestate.get_bounds gs') pos' then
          Placing (btype,pos')
        else Placing (btype,(x,y))
      | _ -> t.act
  } in (t',gs')

let wrap max n =
  let r = n mod max in
  if r >= 0 then r else r + max

let receive_picking c n t gs =
  let types = gs |> Gamestate.get_game_data |> GameData.building_types in
  let n' = wrap
      (List.length types) 
      (match c with
       | Left -> n-1
       | Right -> n+1
       | _ -> n)
  in
  let t' = {
    t with act = match c with
      | Cancel -> Observing
      | Select -> Placing (List.nth types n, center_of_map gs)
      | _ -> BuildingPicker n'
  }
  in (t',gs)

let receive_command c t gs = match t.act with
  | Observing -> receive_observing c t gs
  | Placing (btype, pos) -> receive_placing c btype pos t gs
  | BuildingPicker n -> receive_picking c n t gs
(* | _ -> (t,gs) *)