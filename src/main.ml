(* TODO: from stackoverflow. allowed? *)
let get1char () =
  let termio = Unix.tcgetattr Unix.stdin in
  let () =
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
      { termio with Unix.c_icanon = false } in
  let res = input_char stdin in
  Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio;
  res


let rec play gs =
  Renderer.draw Renderer.temp_input_state gs;
  ignore (get1char () );
  if Gamestate.alive gs then play gs

let () = 
  (* TODO: load gamestate from file *)
  let gs = Gamestate.init_state in
  play gs