open GameData
open GameState

type game = t

type placing_state = PickLocation | AssignWorkers of int
type adjust_workers_state = Assign | Unassign

type action = 
  | Observing
  | Placing of (placing_state * building_type * coordinates)
  | BuildingPicker of int
  | Inspecting of coordinates
  | AdjustWorkers of (adjust_workers_state * coordinates * int)
  | QuitConfirm
  | Quit

type t = {
  msg : string;
  act : action;
}

type command = 
  | New
  | Load
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
  | Save
  | Quit
  | Unrecognized

let starting = {
  msg = "Welcome to Camlished. Best of luck growing your society!"; 
  act = Observing
}


let controls_text t =
  let pairs = match t.act with
    | Observing ->
      [("Space" , "end turn" );("I" , "inspect" );("B" , "place building" );
       ("P" , "save" );("Q", "quit")]
    | Placing (PickLocation,_,_) ->[("W/A/S/D","move");("C","cancel");
                                    (";","select")]
    | Placing (AssignWorkers _,_,_)
    | AdjustWorkers _ ->[("W/S","pick");("C","cancel");(";","select")]
    | Inspecting _ ->[("W/A/S/D","move");("C","cancel");("Y","add workers");
                      ("U","remove workers")]
    | BuildingPicker _ ->[("A/D","pick");("C","cancel");(";","select")]
    | QuitConfirm -> [("C","cancel");(";","confirm")]
    | Quit -> failwith "controls_text invalid for Quit state"
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

let wrap_control max n c = wrap max (
    match c with
    | Up -> n+1
    | Down -> n-1
    | _ -> n
  )

let in_bounds (width,height) (x,y) =
  (x >= 0) && (x < width) && (y >= 0) && (y < height)

let move_control (x,y) gs c =
  let move = match c with
    | Up -> (x,y-1)
    | Down -> (x,y+1)
    | Left -> (x-1,y)
    | Right -> (x+1,y)
    | _ -> (x,y) in
  if in_bounds (get_bounds gs) move then move else (x,y)

let center_of_map gs = match get_bounds gs with
  | (x,y) -> (x/2, y/2)


let comma_fold_opt l = match l with
  | [] -> None
  | h::k ->
    Some (List.fold_left
            (fun acc b -> acc^", "^b)
            h k)


let building_gen_text b gs e = 
  let active_gen = active_gen_rsc_amt b gs in
  let con_gen = con_gen_rsc_amt b gs in
  let write_rsc (r:resource) = (string_of_int r.amount)^" "^r.id in
  let active_gen_text = List.map write_rsc active_gen |> comma_fold_opt in
  let write_con_gen (p,c) = (write_rsc p)^ " for "^(write_rsc c) in
  let con_gen_text = List.map write_con_gen con_gen |> comma_fold_opt in
  match active_gen_text, con_gen_text with
  | Some p, Some c -> e^"producing "^p^", "^c^" per turn."
  | Some s, None| None, Some s -> e^"producing "^s^" per turn."
  | None, None -> e


let building_msg (b:building) gs =
  let gd = get_game_data gs in
  let max_workers = max_workers b.building_type gd in
  let current_workers = List.length b.workers in
  let max_residents = max_residents b.building_type gd in
  let current_residents = List.length b.residents in
  b.building_type^": "
  |> building_gen_text b gs
  |> (fun e -> 
      if max_workers > 0 then
        e^" Workers: "
        ^string_of_int current_workers^"/"^string_of_int max_workers^"."
      else e)
  |> (fun e -> 
      if max_residents > 0 then
        e^" Residents: "
        ^string_of_int current_residents^"/"^string_of_int max_residents^"."
      else e) 

let get_inspect_msg pos gs =
  match get_building_at pos gs with
  | Some b -> building_msg b gs
  | None ->
    begin match get_tile_at pos gs with
      | Some Water -> "Water."
      | Some Forest -> "Forest."
      | Some Mountain -> "Mountain."
      | _ -> "Nothing special here."
    end

let get_building_pick_msg btype gs =
  let gd = get_game_data gs in
  let placement_costs = rsc_requirements btype gd in
  let min_workers = min_req_workers btype gd in
  let write (b:requirement) =
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


let get_building_place_msg btype gs =
  let gd = get_game_data gs in
  let reqs = placement_requirements btype gd in
  let tile_to_str = function
    | Grass -> "grass"
    | Mountain -> "mountains"
    | Forest -> "forest"
    | Water -> "water"
  in
  let write_rule (r:placement_rule) = 
    let tile_str = tile_to_str r.tile in
    match r.rule_type with
    | On -> "on "^tile_str
    | Next -> "next to "^tile_str
  in
  let req_txt = match comma_fold_opt (List.map write_rule reqs) with
    | Some s -> "Must be placed "^s^"."
    | None -> ""
  in "Where will you place this building? " ^ req_txt



let get_assign_workers_msg btype gs =
  let gd = get_game_data gs in
  let max_workers = max_workers btype gd in
  "Max: " ^ (string_of_int max_workers)


let get_adjust_workers_msg pos gs =
  let btype= Option.get (get_building_type_at pos gs) in
  let gd = get_game_data gs in
  let max_workers = max_workers btype gd in
  let b = Option.get (get_building_at pos gs) in
  let current = List.length b.workers in
  "Max: " ^ (string_of_int max_workers) ^
  ", Current: " ^ (string_of_int current)

let receive_observing c t msg_r gs =
  if c = Save then
    begin
      save gs;
      msg_r := "Game saved."
    end;
  let gs' = match c with
    | Step -> step gs
    | _ -> gs in
  let t' = {
    t with act = match c with
      | PlaceBuilding -> BuildingPicker 0
      | Inspect ->
        let center= center_of_map gs in Inspecting center
      | Quit -> QuitConfirm
      | _ -> Observing
  } in (t',gs')

let max_assign_type (btype:building_type) gs =
  let gd = get_game_data gs in
  let max_workers = max_workers btype gd in
  unassigned_workers gs |> List.length
  |> min max_workers

let max_assign (b:building) gs =
  let gd = get_game_data gs in
  let max_workers = max_workers b.building_type gd in
  unassigned_workers gs |> List.length
  |> min (max_workers - (b.workers |> List.length))


let receive_placing_workers c amt btype pos t _ gs =
  let limit = max_assign_type btype gs in
  let amt' = wrap_control (limit+1) amt c in
  let gs' = match c with
    | Select ->
      place_building btype pos gs
      |> assign_workers_c pos amt'
    | _ -> gs
  in let t' = {
      t with
      act = match c with
        | Cancel | Select -> Observing
        | _ -> Placing (AssignWorkers amt', btype, pos)
    } in (t',gs')

let receive_placing_location c btype pos t _ gs =
  let pos' = move_control pos gs c in
  let t' = {
    t with act = match c with
      | Cancel -> Observing
      | Select when can_place_building_at btype pos gs ->
        Placing (AssignWorkers 0,btype,pos')
      | Up | Down | Left | Right -> Placing (PickLocation,btype,pos')
      | _ -> t.act
  } in (t',gs)

let receive_picking c n t _ gs =
  let types = gs |> get_game_data |> buildable_building_types in
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
      | Select when meets_rsc_reqs btype gs ->
        Placing (PickLocation, btype, center_of_map gs)
      | _ -> BuildingPicker n'
  }
  in (t',gs)

let adjust_cmd_to_state : command -> adjust_workers_state = function
  | Assign -> Assign
  | Unassign -> Unassign
  | _ -> failwith "impossible"


let receive_inspect c pos t _ gs =
  let pos' = move_control pos gs c in
  let t' = {
    t with act = match c with
      | Cancel -> Observing
      | Assign | Unassign ->
        begin match get_building_at pos gs with
          | Some _ -> AdjustWorkers (adjust_cmd_to_state c, pos, 0)
          | None -> t.act
        end
      | _ -> Inspecting pos'
  } in
  (t',gs)

let receive_adjust_workers c (state:adjust_workers_state) pos amt t _ gs =
  let b = get_building_at pos gs |> Option.get in
  let limit = match state with
    | Assign -> max_assign b gs
    | Unassign -> List.length b.workers in
  let amt' = wrap_control (limit+1) amt c in
  let gs' = match c with
    | Select ->
      begin match state with 
        | Assign -> assign_workers_c pos amt' gs
        | Unassign -> unassign_workers_c pos amt' gs
      end
    | _ -> gs
  in let t' = { t with
                act = match c with
                  | Cancel | Select -> Observing
                  | _ -> AdjustWorkers (state, pos, amt')
              }
  in (t',gs')


let receive_quit_confirm c (t:t) gs = 
  let t' = 
    { t with act = match c with 
          | Select -> Quit
          | Cancel -> Observing
          | _ -> t.act
    }
  in (t',gs)


let command_state_update c t gs msg_r = match t.act with
  | Observing -> receive_observing c t msg_r gs
  | Placing (PickLocation, b, pos) ->
    receive_placing_location c b pos t msg_r gs
  | Placing (AssignWorkers amt, b, pos) ->
    receive_placing_workers c amt b pos t msg_r gs
  | BuildingPicker n -> receive_picking c n t msg_r gs
  | Inspecting pos -> receive_inspect c pos t msg_r gs
  | AdjustWorkers (state,pos,amt) ->
    receive_adjust_workers c state pos amt t msg_r gs
  | QuitConfirm -> receive_quit_confirm c t gs
  | Quit -> failwith "can't receive command on Quit input state"


let receive_command c t gs =
  let msg_r = ref "" in
  let (input,gs) = command_state_update c t gs msg_r in
  begin match input.act with
    | Inspecting pos -> msg_r := get_inspect_msg pos gs
    | BuildingPicker n ->
      let types = gs |> get_game_data |> buildable_building_types in
      msg_r := get_building_pick_msg (List.nth types n) gs
    | Placing (PickLocation, btype,_) ->
      msg_r := get_building_place_msg btype gs
    | Placing (AssignWorkers _,btype,_) ->
      msg_r := "How many workers should be assigned to this building?"
               ^" "^get_assign_workers_msg btype gs 
    | AdjustWorkers (_,pos,_) -> msg_r := get_adjust_workers_msg pos gs
    | QuitConfirm -> msg_r := "Are you sure you want to quit?"
    | _ -> ()
  end; ( { input with msg = !msg_r },gs)