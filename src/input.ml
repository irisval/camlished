type game = Gamestate.t

type placing_state = PickLocation | AssignWorkers of int
type adjust_workers_state = Assign | Unassign

type action = 
  | Observing
  | Placing of (placing_state * GameData.building_type * Gamestate.coordinates)
  | BuildingPicker of int
  | Inspecting of Gamestate.coordinates
  | AdjustWorkers of (adjust_workers_state * Gamestate.coordinates * int)

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
  | Inspect
  | Assign
  | Unassign
  | Quit
  | Unrecognized

let starting = {
  msg = "Welcome to Camlished!";
  act = Observing
}

let controls_text t =
  let pairs = match t.act with
    | Observing ->
      [
        ("Space" , "end turn" );
        ("I" , "inspect" );
        ("B" , "place building" );
        ("Q", "quit")
      ]
    | Placing (PickLocation,_,_) ->
      [
        ("W/A/S/D","move");
        ("C","cancel");
        (";","select")
      ]
    | Placing (AssignWorkers _,_,_)
    | AdjustWorkers _ ->
      [
        ("W/S","pick");
        ("C","cancel");
        (";","select")
      ]
    | Inspecting _ ->
      [
        ("W/A/S/D","move");
        ("C","cancel");
        ("Y","assign workers");
        ("U","unassign workers")
      ]
    | BuildingPicker _ ->
      [
        ("A/D","pick");
        ("C","cancel");
        (";","select")
      ]
  in
  match pairs with
  | [] -> ""
  | (hl,hr)::k ->
    List.fold_left (fun acc (l,r) -> acc^" | "^l^" : "^r) (hl^" : "^hr) k

let wrap max n =
  if max = 0 then 0 else
    let r = n mod max in
    if r >= 0 then r else r + max

let bad_command_text = "Unrecognized command."

let center_of_map gs = match Gamestate.get_bounds gs with
  | (x,y) -> (x/2, y/2)

let get_inspect_msg (x,y) gs = "hi"

let receive_observing c t msg_r gs =
  let gs' = match c with
    | Step -> Gamestate.step gs
    | _ -> gs in
  let t' = {
    t with act = match c with
      | PlaceBuilding -> BuildingPicker 0
      | Inspect ->
        let center= center_of_map gs in Inspecting center
      | _ -> Observing
  } in (t',gs')

let in_bounds (width,height) (x,y) =
  (x >= 0) && (x < width) && (y >= 0) && (y < height)


let receive_placing_workers c amt btype pos t msg_r gs =
  let amt' = wrap (Gamestate.unassigned_workers gs |> List.length) (
      match c with
      | Up -> amt+1
      | Down -> amt-1
      | _ -> amt
    ) in
  let gs' = match c with
    | Select ->
      Gamestate.place_building btype pos gs
      |> Gamestate.assign_workers pos amt'
    | _ -> gs
  in let t' = {
      t with
      act = match c with
        | Cancel | Select -> Observing
        | _ -> Placing (AssignWorkers amt', btype, pos)
    } in (t',gs')

let receive_placing_location c btype (x,y) t msg_r gs =
  let pos' = 
    let move = match c with
      | Up -> (x,y-1)
      | Down -> (x,y+1)
      | Left -> (x-1,y)
      | Right -> (x+1,y)
      | _ -> (x,y) in
    if in_bounds (Gamestate.get_bounds gs) move then move else (x,y)
  in
  let t' = {
    t with act = match c with
      | Cancel -> Observing
      | Select when Gamestate.can_place_building_at btype (x,y) gs ->
        Placing (AssignWorkers 0,btype,pos')
      | Up | Down | Left | Right -> Placing (PickLocation,btype,pos')
      | _ -> t.act
  } in (t',gs)

let receive_picking c n t msg_r gs =
  let types = gs |> Gamestate.get_game_data |> GameData.building_types in
  let n' = wrap
      (List.length types) 
      (match c with
       | Left -> n-1
       | Right -> n+1
       | _ -> n)
  in
  let btype = List.nth types n in
  let t' = {
    t with act = match c with
      | Cancel -> Observing
      | Select when Gamestate.can_place_building btype gs ->
        Placing (PickLocation, btype, center_of_map gs)
      | _ -> BuildingPicker n'
  }
  in (t',gs)

let adjust_cmd_to_state : command -> adjust_workers_state = function
  | Assign -> Assign
  | Unassign -> Unassign
  | _ -> failwith "impossible"


let receive_inspect c (x,y) t msg_r gs =
  let pos' = 
    let move = match c with
      | Up -> (x,y-1)
      | Down -> (x,y+1)
      | Left -> (x-1,y)
      | Right -> (x+1,y)
      | _ -> (x,y) in
    if in_bounds (Gamestate.get_bounds gs) move then move else (x,y)
  in
  let t' = {
    t with act = match c with
      | Cancel -> Observing
      | Assign | Unassign ->
        begin match Gamestate.get_building_at (x,y) gs with
          | Some _ -> AdjustWorkers (adjust_cmd_to_state c, (x,y), 0)
          | None -> t.act
        end
      | _ -> Inspecting pos'
  } in
  (t',gs)

let receive_adjust_workers c (state:adjust_workers_state) pos amt t msg_r gs =
  let b = match Gamestate.get_building_at pos gs with
    | Some x -> x
    | None -> failwith "impossible" in
  let limit = match state with
    | Assign -> Gamestate.unassigned_workers gs |> List.length
    | Unassign -> Gamestate.get_workers_at b |> List.length in
  let amt' = wrap limit (
      match c with
      | Up -> amt+1
      | Down -> amt-1
      | _ -> amt
    ) in
  let gs' = match c with
    | Select ->
      begin match state with 
        | Assign -> Gamestate.assign_workers pos amt' gs
        | Unassign -> Gamestate.unassign_workers pos amt' gs
      end
    | _ -> gs
  in let t' = {
      t with
      act = match c with
        | Cancel | Select -> Observing
        | _ -> AdjustWorkers (state, pos, amt')
    } in (t',gs')


let receive_command c t gs =
  let msg_r = ref "" in
  let (input,gs) = match t.act with
    | Observing -> receive_observing c t msg_r gs
    | Placing (PickLocation, b, pos) ->
      receive_placing_location c b pos t msg_r gs
    | Placing (AssignWorkers amt, b, pos) ->
      receive_placing_workers c amt b pos t msg_r gs
    | BuildingPicker n -> receive_picking c n t msg_r gs
    | Inspecting pos -> receive_inspect c pos t msg_r gs
    | AdjustWorkers (state,pos,amt) ->
      receive_adjust_workers c state pos amt t msg_r gs
  in
  begin match input.act with
    | Inspecting pos ->
      msg_r := get_inspect_msg pos gs
    | BuildingPicker _ ->
      msg_r := "Which type of building?"
    | Placing (PickLocation, _,_) ->
      msg_r := "Where will you put this building?"
    | _ -> ()
  end;
  ( { input with msg = !msg_r },gs)