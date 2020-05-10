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
        ("Y","add workers");
        ("U","remove workers")
      ]
    | BuildingPicker _ ->
      [
        ("A/D","pick");
        ("C","cancel");
        (";","select")
      ]
  in
  let write (l,r) = l^" : "^r in
  match pairs with
  | [] -> ""
  | h::k ->
    List.fold_left (fun acc p -> acc^" | "^write p) (write h) k

let wrap max n =
  if max = 0 then 0 else
    let r = n mod max in
    if r >= 0 then r else r + max

let bad_command_text = "Unrecognized command."

let center_of_map gs = match Gamestate.get_bounds gs with
  | (x,y) -> (x/2, y/2)


let get_inspect_msg pos gs =
  let gd = Gamestate.get_game_data gs in
  let building_msg (b:Gamestate.building) =
    let max_workers = GameData.max_workers b.building_type gd in
    let current_workers = List.length b.workers in
    let show_workers = max_workers > 0 in
    let max_residents = GameData.max_residents b.building_type gd in
    let current_residents = List.length b.residents in
    let show_residents = max_residents > 0 in
    let resources = "" in
    let producing = "per turn" in
    let consuming = Some "" in
    b.building_type^": "^
    "producing "
    ^producing
    |> (fun e ->
        match consuming with
        | Some x -> e^" for "^x
        | None -> e
      )
    |> (fun e -> e^".")
    |> (fun e -> 
        if show_workers then
          e^" Workers: "
          ^string_of_int current_workers^"/"^string_of_int max_workers^"."
        else e)
    |> (fun e -> 
        if show_residents then
          e^" Residents: "
          ^string_of_int current_residents^"/"^string_of_int max_residents^"."
        else e) 
  in
  match Gamestate.get_building_at pos gs with
  | Some b -> building_msg b
  | None -> "Nothing special here"

let get_building_pick_msg btype gs =
  let gd = Gamestate.get_game_data gs in
  let placement_costs = GameData.rsc_requirements btype gd in
  let min_workers = GameData.min_req_workers btype gd in
  let write (b:GameData.requirement) =
    (string_of_int b.amount)^" "^b.resource in
  "Requires: "
  ^ match placement_costs with
  | [] -> "Nothing"
  | h::k ->
    List.fold_left (fun acc b -> acc ^ ", " ^ write b) (write h) k ^"."
    (* |> TODO: show placement requirement? *)
    |> (fun e -> 
        if min_workers > 0 then
          e^" Minimum workers: "
          ^string_of_int min_workers^"."
        else e^".")
    |> (fun e -> e)

let get_assign_workers_msg btype gs =
  let gd = Gamestate.get_game_data gs in
  let max_workers = GameData.max_workers btype gd in
  "Max: " ^ (string_of_int max_workers)


let get_adjust_workers_msg pos gs =
  let btype= Option.get (Gamestate.get_building_type_at pos gs) in
  let gd = Gamestate.get_game_data gs in
  let max_workers = GameData.max_workers btype gd in
  let b = Option.get (Gamestate.get_building_at pos gs) in
  let current = List.length b.workers in
  "Max: " ^ (string_of_int max_workers) ^
  ", Current: " ^ (string_of_int current)

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

let max_assign_type (btype:GameData.building_type) gs =
  let gd = Gamestate.get_game_data gs in
  let max_workers = GameData.max_workers btype gd in
  Gamestate.unassigned_workers gs |> List.length
  |> min max_workers

let max_assign (b:Gamestate.building) gs =
  let gd = Gamestate.get_game_data gs in
  let max_workers = GameData.max_workers b.building_type gd in
  Gamestate.unassigned_workers gs |> List.length
  |> min (max_workers - (b.workers |> List.length))


let receive_placing_workers c amt btype pos t msg_r gs =
  let limit = max_assign_type btype gs in
  let amt' = wrap (limit+1) (
      match c with
      | Up -> amt+1
      | Down -> amt-1
      | _ -> amt
    ) in
  let gs' = match c with
    | Select ->
      Gamestate.place_building btype pos gs
      |> Gamestate.assign_workers_c pos amt'
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
      | Select when Gamestate.meets_rsc_reqs btype gs ->
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
  let b = Gamestate.get_building_at pos gs |> Option.get in
  let limit = match state with
    | Assign -> max_assign b gs
    | Unassign -> List.length b.workers in
  let amt' = wrap (limit+1) (
      match c with
      | Up -> amt+1
      | Down -> amt-1
      | _ -> amt
    ) in
  let gs' = match c with
    | Select ->
      begin match state with 
        | Assign -> Gamestate.assign_workers_c pos amt' gs
        | Unassign -> Gamestate.unassign_workers_c pos amt' gs
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
    | BuildingPicker n ->
      let types = gs |> Gamestate.get_game_data |> GameData.building_types in
      msg_r := get_building_pick_msg (List.nth types n) gs
    | Placing (PickLocation, _,_) ->
      msg_r := "Where will you put this building?"
    | Placing (AssignWorkers _,btype,_) ->
      msg_r := "How many workers should be assigned to this building?"
               ^" "^get_assign_workers_msg btype gs 
    | AdjustWorkers (_,pos,_) ->
      msg_r := get_adjust_workers_msg pos gs
    | _ -> ()
  end;
  ( { input with msg = !msg_r },gs)