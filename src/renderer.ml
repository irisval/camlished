type input = Input.t
type game = Gamestate.t

open Gamestate
open GameData
open ANSITerminal

(**[char_of_building b] is [(s,ch)] where [ch] is the char representing [b],
   [s] is its style features.*)
let char_of_building = function
  | "Tent" -> ([white;on_black], '&')
  | "Wood Shelter" -> ([white;on_black], 'T')
  | "Log Cabin" -> ([white;on_black], 'H')
  | "Sawmill" -> ([white;on_black],'*')
  | "Castle" -> ([white;on_black],'M')
  | "Quarry" -> ([white;on_black],'Q')
  | "Mine" -> ([white;on_black],'_')
  | "Forester" -> ([white;on_black],'n')
  | "Forge" -> ([white;on_black],'l')
  | "Fishing Docks" -> ([white;on_black],'=')
  | "Farm" -> ([white;on_yellow],' ')
  | _ -> ([black;on_white],'?')

(**[char_of_tile t] is [(s,ch)] where [ch] is the char representing [t],
   [s] is its style features.*)
let char_of_tile = function
  | Grass -> ([on_green], ' ')
  | Rock -> ([Bold;black;on_green],'^')
  | Water -> ([white;on_blue],if Random.bool () then '~' else ' ')
  | Trees -> ([black;on_green],'l')


let input_map_overlay (current_style,current) (x,y) (input:Input.t) gs = 
  match input.act with
  | Input.Placing (t,(xp,yp)) ->
    begin match (xp = x,yp = y) with
      | (true,true) ->
        let (s,c) = char_of_building t in
        if can_place_building_at t (xp,yp) gs then
          Some (s,c)
        else Some (s@[on_red],c)
      | (true,false) -> Some (current_style@[white], '|')
      | (false,true) -> Some (current_style@[white], '-')
      | _ -> None
    end
  | Input.Inspecting (xp,yp) ->
    begin match (xp = x,yp = y) with
      | (true,true) -> Some (current_style,current)
      | (true,false) -> Some (current_style@[white], '|')
      | (false,true) -> Some (current_style@[white], '-')
      | _ -> None
    end
  | _ -> None

(**[char_at pos gs] is [(s,ch)] where [ch] is the char representing the
   tile at [pos] in [gs], [s] is its style features.*)
let char_at pos input gs =
  let base = match Gamestate.get_building_type_at pos gs
    with
    | Some b -> char_of_building b
    | None -> char_of_tile (Gamestate.tile_rep_at pos gs)
  in
  let over = input_map_overlay base pos input gs in
  match over with 
  | Some b -> b
  | None -> base



(**[repeat_list n x] is a list of [x] repeated [n] times. *)
let rec repeat_list n x =
  if (n = 0) then [] else x::(repeat_list (n-1) x)


(**[add_border arr width] is [arr] with a border added surrounding it, where
   [width] is the width of the array. *)
let add_border arr width =
  let top = ([],'\\')::repeat_list (width)  ([],'-')@[([],'/')]
            |> Array.make 1  in
  let bot = ([],'/')::repeat_list (width)  ([],'-')@[([],'\\')]
            |> Array.make 1  in
  Array.concat [
    top;
    Array.map (fun x -> ([],'|')::x@[([],'|')]) arr;
    bot;
  ]


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
  let raw_map = draw_aux arr 0 in
  add_border raw_map width 

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

let last n s = List.rev (first n (List.rev s))

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
       (fun e ->
          let style =
            (if e = selected then [Inverse] else [])
            @(if can_place_building e gs then [] else [red])
          in style_string style e
       )
     |> List.fold_left (fun acc b -> acc@([],' ')::b) []
    )
    arr

let add_turn x y gs arr = 
  let s = "Turn: "^(Gamestate.get_turn gs |> string_of_int) in
  add_text x y (style_string [cyan] s) arr


let hide_cursor () = printf [] "\027[?25l%!"

(**[print_2d o] prints [o] to the console with each element of [o] as a
   separate line.*)
let print_2d o =
  List.iter
    (fun line -> List.iter
        (fun (s,c) -> print_string s (c |> String.make 1 ) )
        line; print_newline ())
    (Array.to_list o)

let draw (input:Input.t) gs = 
  let (width,height) = Gamestate.get_bounds gs in
  let output = ref (text_map input gs) in
  let map_bottom = height + 3 in
  let map_right = width + 2 in
  output := 
    Array.append (Array.make 1 []) !output
    |> add_resources (map_right + 3) 1 gs
    |> add_turn 0 0 gs
    |> add_message (map_bottom) [] input.msg
    |> (
      fun o -> match input.act with
        | Input.BuildingPicker n -> add_building_picker n (map_bottom+1) gs o
        | _ -> o
    );
  erase Screen;
  hide_cursor ();
  set_cursor 1 1;
  print_2d !output
