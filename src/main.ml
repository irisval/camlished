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
  | "l" -> Input.Load
  | "n" -> Input.New
  | _ -> Input.Unrecognized


let rec play in_state gs =
  Renderer.draw in_state gs;
  let c = read_char () |> char_to_command in
  let is_quit = match c with Quit -> true | _ -> false in
  if is_quit then exit 0 else
    let (in_state', gs') = Input.receive_command c in_state gs in
    if Gamestate.alive gs then play in_state' gs'
    else Renderer.you_died in_state' gs'

(** [load_saved_data f] reads in the data file t [f]. Gives an error if [f] is
    either not found or has an invalid JSON representation. *)
let load_saved_data f =
  try 
    let gs = (f |> Yojson.Basic.from_file |> Gamestate.from_json) in
    play Input.starting gs
  with
  | Yojson.Json_error f -> 
    ANSITerminal.(print_string [red] "Error parsing saved game data. Please make
     sure that the file is in valid JSON format.")
  | Sys_error f -> 
    ANSITerminal.(print_string [red] "File not found.")

let () = 
  ANSITerminal.(print_string [green] "Welcome to Camlished!\n
  Please press l to load a game file or n to start a new game.\n");
  print_endline "";
  let c = read_char () |> char_to_command in 
  let is_new = match c with New -> true | _ -> false in
  if is_new then 
    let gs = Gamestate.initial_state () in play Input.starting gs
  else 
    ANSITerminal.(print_string [green] "\nPlease enter the location of your game data.
      \n Ex: 'src/sampleSavedState.json' ");
    match read_line () with
    | exception End_of_file -> ()
    | file_name -> load_saved_data file_name

