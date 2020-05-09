let read_char () =
  let io = Unix.tcgetattr Unix.stdin in
  let () =
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
      { io with Unix.c_icanon = false } in
  let o = input_char stdin in
  Unix.tcsetattr Unix.stdin Unix.TCSADRAIN io;
  o

let char_to_command c =
  match Char.escaped c with
  | " " -> Input.Step
  | "b" -> Input.PlaceBuilding
  | "c" -> Input.Cancel
  | ";" -> Input.Select
  | "i" -> Input.Inspect
  | "w" -> Input.Up
  | "s" -> Input.Down
  | "a" -> Input.Left
  | "d" -> Input.Right
  | "y" -> Input.Assign
  | "u" -> Input.Unassign
  | "q" -> Input.Quit
  | _ -> Input.Unrecognized


let rec play in_state gs =
  Renderer.draw in_state gs;
  let c = read_char () |> char_to_command in
  let is_quit = match c with Quit -> true | _ -> false in
  if is_quit then exit 0 else
    let (in_state',gs') = Input.receive_command c in_state gs in
    if Gamestate.alive gs then play in_state' gs'

let () = 
  let gs = ("src/sampleSavedState.json" |> Yojson.Basic.from_file |> Gamestate.from_json) in
  play Input.starting gs