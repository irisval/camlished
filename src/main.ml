let start f =
  try 
    let w = f |> Yojson.Basic.from_file |> Gamestate.from_json in 
    ANSITerminal.(print_string [red] "for testing purposes")
  with
  | Yojson.Json_error _ -> 
    ANSITerminal.(print_string [red] "not a valid JSON file.")
  | Sys_error _ -> 
    ANSITerminal.(print_string [red] "file not found.")


let () = ANSITerminal.(print_string [red] "load ur file");
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> start file_name
