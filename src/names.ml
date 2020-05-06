(** [vowels] is the list of vowels *)
let vowels = ['a'; 'e'; 'i'; 'o'; 'u']

(* types to define a random name config *)
type v_or_c = Vowel | Consonant
type char_config = float * (v_or_c list)
type config_list = char_config list

(* config based on Krzmrgystan General Hospital *)
let patient = [
  (1.0, [Consonant]);
  (1.0, [Vowel]);
  (0.5, [Consonant]);
  (1.0, [Consonant]);
  (1.0, [Vowel]);
  (0.5, [Consonant]);
  (0.5, [Consonant; Vowel]);
]

let init = Random.init

(** [rand_char ()] is a random character *)
let rand_char () =
  Char.chr (97 + Random.int 26)

(** [rand_vowel ()] is a random vowel *)
let rec rand_vowel () =
  let c = rand_char () in
  if List.mem c vowels then c
  else rand_vowel ()

(** [rand_const ()] is a random consonant *)
let rec rand_const () =
  let c = rand_char () in
  if not (List.mem c vowels) then c
  else rand_const ()

(** [assemble_vc_list l] turns [l] into random vowels and consonants *)
let rec assemble_vc_list l =
  match l with
  | [] -> []
  | h::t ->
    match h with
    | Vowel -> (rand_vowel ())::(assemble_vc_list t)
    | Consonant -> (rand_const ())::(assemble_vc_list t)

(** [proc_char_config conf] processes [conf] into a list of random v, c *)
let rec proc_char_config conf =
  match conf with
  | [] -> []
  | (chance, vc)::t ->
    if Random.float 1.0 < chance then
      (assemble_vc_list vc)@(proc_char_config t)
    else
      proc_char_config t

(** [to_stringname l] converts [l] to a string *)
let to_stringname l =
  List.fold_left (fun acc c -> acc ^ Char.escaped c) "" l

(** [make_name conf] uses [conf] to make a capitalized string name *)
let make_name conf =
  conf |> proc_char_config |> to_stringname |> String.capitalize_ascii

let rand_partname () =
  make_name patient

let rand_fullname () =
  rand_partname () ^ " " ^ rand_partname ()

