open Lib
open ANSITerminal

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
  | "p" -> Input.Save
  | _ -> Input.Unrecognized


let rec play in_state gs =
  Renderer.draw in_state gs;
  let c = read_char () |> char_to_command in
  let (in_state', gs') = Input.receive_command c in_state gs in
  match in_state'.act with
  | Quit -> exit 0
  | _ ->
    if GameState.alive gs' then 
      play in_state' gs'
    else Renderer.you_died in_state' gs'; exit 0

(** [load_saved_data f] reads in the data file t [f]. Gives an error if [f] is
    either not found or has an invalid JSON representation. *)
let load_saved_data f =
  try 
    let gs = (f |> Yojson.Basic.from_file |> GameState.from_json) in
    play Input.starting gs
  with
  | Yojson.Json_error _ -> 
    ANSITerminal.(print_string [red]
                    "Error parsing saved game data.
Please make sure that the file is in valid JSON format.")
  | Sys_error _ -> 
    ANSITerminal.(print_string [red] "File not found.")



let rec pick_game () =
  Renderer.full_clear ();
  print_string [green] "Welcome to Camlished!\n
  Please press 'L' to load a game file or 'N' to start a new game.\n";
  print_endline "";
  let c = read_char () |> char_to_command in 
  match c with
  | New -> new_game ()
  | Load -> load_game ()
  | _ -> (print_string [green] "\nPress 'L' or 'N' nerd");
    pick_game ()

and new_game () = 
  Renderer.full_clear ();
  (print_string [green] "Please enter the name of your world!\n");
  print_endline "";
  let name = read_line () in 
  let gs = GameState.initial_state name in play Input.starting gs 

and load_game () =
  Renderer.full_clear ();
  (print_string [green]
     "\nPlease enter the location of your game data.
      \n Ex: 'saves/sampleSavedState.json' ");
  begin match read_line () with
    | exception End_of_file -> ()
    | file_name -> load_saved_data file_name
  end

let () = pick_game ()