type input_state = unit

open Gamestate
open ANSITerminal

type game_state = Gamestate.t

(**[char_of_building b] is [(s,ch)] where [ch] is the char representing [b],
   [s] is its style features.*)
let char_of_building = function
  | _ -> ([], 'M')

(**[char_of_tile t] is [(s,ch)] where [ch] is the char representing [t],
   [s] is its style features.*)
let char_of_tile = function
  | Grass -> ([green], ' ')
  | Rock -> ([green],'~')

(**[char_at pos gs] is [(s,ch)] where [ch] is the char representing the
   tile at [pos] in [gs], [s] is its style features.*)
let char_at pos gs =
  match Gamestate.get_building_type_at pos gs
  with
  | Some b -> char_of_building b
  | None -> char_of_tile (Gamestate.get_tile_at pos gs) 


(**[text_map input gs] gives the text representation of the map in [gs] 
    while in input state [input].*)
let text_map _ gs =
  let bounds = Gamestate.get_bounds gs in
  let width = fst bounds in
  let height = snd bounds in
  let draw_row y =
    let rec draw_row_aux acc x =
      let c = char_at (x,y) gs in
      let acc' = (acc@[c]) in
      if (x+1 < width) then draw_row_aux acc' (x+1) else acc'
    in
    draw_row_aux [] 0;
  in
  let rec draw_aux acc y = 
    let row = draw_row y in
    let acc' = (row::acc) in
    if(y+1 < height) then draw_aux acc' (y+1) else acc'
  in
  draw_aux [] 0


let string_to_list str = (Str.split (Str.regexp "[^0-9]+") str)

(**[resource_label (name,value)] gives the rendered label for a resource named
    [name] with quantity [value].*)
let resource_label (name,value) = 
  (name ^ ": " ^ string_of_int value)
  |> string_to_list
  |> List.map (fun e -> ([magenta],e))

(**[insert_at s pos x] is [s] with [x] written into it starting at [pos].
   If [s] is shorter than [n] characters, spaces are added.*)
(* TODO *)
let insert_at s pos x = s

(**[fill_to s n] is [s] with empty strings added to ensure [s] has at least [n]
   elements.*)
let rec fill_to s n = if List.length s >= n then s else s@[]

(* let add_resources dist gs current =
   let resources = Gamestate.get_resources in
   let rec add_resources_aux o line r = 
    match r with
    | [] -> o
    | h::k ->
      insert_at (fill_to o line)
   in
   add_resources_aux current 0 resources  *)


let draw input gs = 
  let output = 
    text_map input gs 
    (* |> add_resources 40 gs *)
  in
  set_autoreset false;
  erase Screen;
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the 3110 Text Adventure Game engine.\n");
  List.iter
    (fun line -> List.iter
        (fun (s,c) -> print_string s (c |> Char.escaped) )
        line; print_newline ())
    output

let temp_input_state = ()