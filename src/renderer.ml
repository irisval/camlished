type input = Input.t
type game = Gamestate.t

open Gamestate
open GameData
open ANSITerminal

(**[char_of_building b] is [(s,ch)] where [ch] is the char representing [b],
   [s] is its style features.*)
let char_of_building = function
  | _ -> ([], 'M')

(**[char_of_tile t] is [(s,ch)] where [ch] is the char representing [t],
   [s] is its style features.*)
let char_of_tile = function
  | Grass -> ([on_green], ' ')
  | Rock -> ([black;on_green],'#')
  | Water -> ([white;on_blue],if Random.bool () then '~' else ' ')
  | Trees -> ([green;on_black],'^')


let input_map_overlay (x,y) (input:Input.t) _ = 
  match input.act with
  | Input.Placing (_,(xp,yp)) ->
    begin match (xp = x,yp = y) with
      | (true,true) -> Some ([white;on_green], ' ')
      | (true,false) -> Some ([white;on_green], '|')
      | (false,true) -> Some ([white;on_green], '-')
      | _ -> None
    end
  | _ -> None

(**[char_at pos gs] is [(s,ch)] where [ch] is the char representing the
   tile at [pos] in [gs], [s] is its style features.*)
let char_at pos input gs =
  let over = input_map_overlay pos input gs in
  match over with 
  | Some b -> b
  | None ->
    begin match Gamestate.get_building_at pos gs
      with
      | Some b -> char_of_building b
      | None -> char_of_tile (Gamestate.tile_rep_at pos gs)
    end


(**[text_map input gs] gives the text representation of the map in [gs] 
   while in input state [input].*)
let text_map input gs =
  let bounds = Gamestate.get_bounds gs in
  let width = fst bounds in
  let height = snd bounds in
  let draw_row y =
    let rec draw_row_aux acc x =
      let c = char_at (x,y) input gs in
      let acc' = (acc@[c]) in
      if (x+1 < width) then draw_row_aux acc' (x+1) else acc'
    in
    draw_row_aux [] 0;
  in
  let rec draw_aux arr y = 
    let row = draw_row y in
    Array.set arr y row;
    if(y+1 < height) then draw_aux arr (y+1) else arr
  in
  let arr = Array.make height [] in
  draw_aux arr 0

let string_to_chars s =
  let rec aux i acc = 
    if( i < 0) then acc else aux (i-1) (s.[i] ::acc) in
  aux (String.length s -1) []

let style_string (l: style list)(s:string) = 
  string_to_chars s |>
  List.map (fun e -> (l,e))

(**[resource_label (name,value)] gives the rendered label for a resource named
   [name] with quantity [value].*)
let resource_label (name,value) = 
  (name ^ ": " ^ string_of_int value)
  |> style_string [magenta] 

let rec first n s = 
  if n = 0 then [] else
    match s with
    | [] -> s
    | h::k -> h :: (first (n-1) k)

let last n s = first n (List.rev s)

let rec fill_to n filler s =
  if List.length s >= n then s
  else fill_to n filler (s@[filler])

let extend n filler arr = 
  let l = Array.length arr in
  if l >= n then arr
  else Array.append arr (Array.make (n-l) filler)


(** [insert_at pos x s] is [s] with [x] written into it starting at [pos].
    If [s] is shorter than [pos] characters, spaces are added.
    If [s] is longer than [pos] characters, characters after [pos] are
    overwritten up to the length of [x].*)
let insert_at pos x s = 
  let len = List.length s in
  (first pos s |> fill_to pos ([],' '))
  @ x
  @ (last (len-pos-(List.length x) |> max 0) s)

let add_resources x y gs arr =
  let resources = (get_user_resources gs) in
  let arr = extend ((List.length resources * 2) + y) [] arr in
  let rec add_resources_aux (line:int) r = 
    match r with
    | [] -> arr
    | h::k ->
      let label = resource_label h in
      let linetxt' =
        Array.get arr line
        |> insert_at x label
      in
      Array.set arr line linetxt';
      let line' = line + 2 in
      add_resources_aux line' k 
  in
  add_resources_aux y resources

let add_text x y styletext arr = 
  let arr = extend (y+1) [] arr in
  let linetxt' =
    Array.get arr y
    |> insert_at x styletext in
  Array.set arr y linetxt';
  arr

let add_message y style msg arr = add_text 0 y (msg |> style_string style) arr

let add_building_picker n y gs arr =
  let types = gs |> Gamestate.get_game_data |> GameData.building_types in
  let selected = List.nth types n in
  add_text 0 y
    (types
     |> List.map
       (fun e -> style_string (if e = selected then [Inverse] else []) e)
     |> List.fold_left (fun acc b -> acc@([],' ')::b) []
    )
    arr


let hide_cursor () = printf [] "\027[?25l%!"

(**[print_2d o] prints [o] to the console with each element of [o] as a
   separate line.*)
let print_2d o =
  List.iter
    (fun line -> List.iter
        (fun (s,c) -> print_string s (c |> Char.escaped) )
        line; print_newline ())
    (Array.to_list o)

let draw (input:Input.t) gs = 
  let (_,height) = Gamestate.get_bounds gs in
  let output = 
    text_map input gs 
    |> add_resources 25 1 gs
    |> add_message height [] input.msg
    |> (
      fun o -> match input.act with
        | Input.BuildingPicker n -> add_building_picker n (height+1) gs o
        | _ -> o
    )
  in
  erase Screen;
  hide_cursor ();
  set_cursor 1 1;
  print_2d output
