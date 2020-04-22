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
  match Gamestate.get_building_at pos gs
  with
  | Some b -> char_of_building b
  | None -> char_of_tile (Gamestate.get_resource_at pos gs) 


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


(* TODO: does not support newline *)
let string_to_list str = (Str.split (Str.regexp "*") str)

(**[resource_label (name,value)] gives the rendered label for a resource named
    [name] with quantity [value].*)
let resource_label (name,value) = 
  (name ^ ": " ^ string_of_int value)
  |> string_to_list
  |> List.map (fun e -> ([magenta],e))

let rec first n s = 
  if n = 0 then s else
    match s with
    | [] -> s
    | h::k -> h :: (first (n-1) k)

let last n s = first n (List.rev s)

let rec fill_to n filler s =
  if List.length s >= n then s
  else fill_to n filler (s@[filler])

(**[insert_at s pos x] is [s] with [x] written into it starting at [pos].
    If [s] is shorter than [n] characters, spaces are added.*)
let insert_at s pos x = 
  let len = List.length s in
  (first pos s |> fill_to pos ([]," "))
  @ x
  @ (last (len-pos |> max 0) s)

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
  erase Screen;
  List.iter
    (fun line -> List.iter
        (fun (s,c) -> print_string s (c |> Char.escaped) )
        line; print_newline ())
    output

let temp_input_state = ()