open OUnit2
open Yojson.Basic.Util
open Gamestate
open GameData

let j = "src/sampleSavedState.json" |> Yojson.Basic.from_file |> Gamestate.from_json

(* source: a2 *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

let building_types: (building_type list) = 
  ["tent"; "wood shelter"; "log cabin"; "castle"; "quarry"; "mine"; "tree farm";
  "forge"; "sawmill"; "fishing dock"; "farm"]
let resource_types: (resource_type list) = ["food"; "stone"; "wood"; "planks";
  "ore"; "metal"]

let unwrap_building_test j = 
  match (get_building_at (5, 6) j) with 
  | Some b -> b
  | None -> failwith "building not found"

let unwrap_building_type_test1 j  = 
  match (get_building_type_at (3, 2) j) with 
  | Some b -> b
  | None -> failwith "building not found"

let unwrap_building_type_test2 j  = 
  match (get_building_type_at (10, 12) j) with 
  | Some _ -> false
  | None -> true

let unwrap_tile j  = 
  match (get_tile_at (1, 2) j) with 
  | Some t -> t
  | None -> failwith "tile not found"


let unwrap_building_placement_test j = 
  match (get_building_at (0, 0) j) with 
  | Some b -> b
  | None -> failwith "building not found"

let state_tests = [
  "Checking tile parsing" >:: 
  (fun _ -> assert_equal  (j |> Gamestate.get_tiles |> List.length) 22);
  "Checking get buildings" >:: 
  (fun _ -> assert (cmp_set_like_lists 
                      (j |> get_game_data |> GameData.building_types) (building_types)));
  "Checking get resources" >:: 
  (fun _ -> assert (cmp_set_like_lists 
                      (j |> get_game_data |> GameData.resource_types) (resource_types)));
  "Checking get building at" >:: 
  (fun _ -> assert_equal (j |> unwrap_building_test) get_test_building);
  "Checking get building type at" >:: 
  (fun _ -> assert_equal (j |> unwrap_building_type_test1) "sawmill");
  "Checking empty get building type at" >:: 
  (fun _ -> assert (j |> unwrap_building_type_test2));
  "Checking get tile at" >:: 
  (fun _ -> assert_equal (j |> unwrap_tile) get_test_tile);
  "Checking is empty" >:: 
  (fun _ -> assert (is_empty (15, 6) j));
  (* "Checking make building" >:: 
  (fun _ -> (assert_equal ((place_building "Silo" (0, 0) j) |> unwrap_building_placement_test)
               get_test_placed_building)); *)
  "Checking population" >:: (fun _ -> (assert_equal (j |> population) 1));
]

let tests = [
  state_tests;
]

let suite = "state test suite" >::: List.flatten tests
let _ = run_test_tt_main suite