(* TODO: from stackoverflow. allowed? *)
let read_char () =
  let termio = Unix.tcgetattr Unix.stdin in
  let () =
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
      { termio with Unix.c_icanon = false } in
  let res = input_char stdin in
  Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio;
  res

let char_to_command c =
  match Char.escaped c with
  | " " -> Input.Step
  | "b" -> Input.PlaceBuilding
  | "c" -> Input.Cancel
  | ";" -> Input.Select
  | "w" -> Input.Up
  | "s" -> Input.Down
  | "a" -> Input.Left
  | "d" -> Input.Right
  | _ -> Input.Unrecognized


let rec play in_state gs =
  Renderer.draw in_state gs;
  let c = read_char () |> char_to_command in
  let (in_state',gs') = Input.receive_command c in_state gs in
  if Gamestate.alive gs then play in_state' gs'

let () = 
  (* TODO: load gamestate from file *)
  let gs = Gamestate.init_state in
  play Input.starting gs