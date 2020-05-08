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
let resource_types: (resource_type list) =
  ["food"; "stone"; "wood"; "planks";
   "ore"; "metal"]


let unwrap_placement_test j coor = 
  match (get_building_at coor j) with 
  | Some b -> b
  | None -> failwith "building not found"

let matches_test_building b = 
  match List.find_opt (fun b'-> b = b') get_test_placed_buildings with 
  | Some _ -> true 
  | None -> false 

let unwrap_test_building j coor = 
  match (get_building_type_at coor j) with 
  | Some b -> b
  | None -> failwith "building not found"


let unwrap_building_type_test2 j  = 
  match (get_building_type_at (10, 12) j) with 
  | Some _ -> false
  | None -> true


let unwrap_tile j coor = 
  match (get_tile_at coor j) with 
  | Some t -> t
  | None -> failwith "tile not found"



(** [unwrap_building b] is [b] but not optional *)
let unwrap_building b =
  match b with
  | Some b -> b
  | None -> failwith "failed to unwrap building"

let tent_1_residents = ["john doe"; "jane doe"]
let tent_2_residents = ["martha"]
let all_residents = tent_1_residents @ tent_2_residents
let assigned_workers = ["john doe"; "martha"]
let unassigned_workers = ["jane doe"]
let sawmill_workers = ["john doe"; "martha"]

let state_tests = [
  (* "Checking tile parsing" >:: (fun _ ->
      assert_equal  (j |> Gamestate.get_tiles |> List.length) 22); *)
  "Checking get building types" >:: (fun _ ->
      assert (cmp_set_like_lists
                (j |> get_game_data |> GameData.building_types) (building_types)));
  "Checking get resource types" >:: (fun _ ->
      assert (cmp_set_like_lists
                (j |> get_game_data |> GameData.resource_types) (resource_types)));
  "Checking get building at" >:: (fun _ ->
      assert_equal  (unwrap_placement_test j (5,6)) get_test_building);
  "Checking get building type at" >:: (fun _ ->
      assert_equal (unwrap_test_building j (3, 2)) "sawmill");
  "Checking empty get building type at" >:: (fun _ ->
      assert (j |> unwrap_building_type_test2));
  "Checking get tile at" >:: (fun _ ->
      assert_equal (unwrap_tile j (1, 2)) get_test_tile); 
  "Checking  is empty" >:: (fun _ ->
      assert (is_empty (15, 6) j));
]

let building_placement_tests = [
  "Checking the successful placement of a building with no requirements" >::
     (fun _ -> let b = place_building "tent" (0, 0) j in 
        assert (matches_test_building (unwrap_placement_test b (0, 0))));
  "Checking the failed placement of a building on an occupied tile" >:: (fun _ -> 
       assert (((is_empty (7, 5) j) |> not) && (can_place_building_at "tent" (7, 5) j) |> not));
  "Checking the successful placement of a building that meets all resource reqs" >::
     (fun _ -> let b = place_building "quarry" (2, 2) j in 
    assert (matches_test_building (unwrap_placement_test b (2, 2))));
  "Checking the failed placement of a building that does not meet all resource reqs" >::
     (fun _ -> assert (((meets_rsc_reqs "forge" j) |> not) 
            && (can_place_building_at "quarry" (6, 6) j) |> not));
  "Checking the placement of a building that meets the 'on tile' placement req" >::
     (fun _ -> let b = place_building "quarry" (3, 3) j in 
    assert (matches_test_building (unwrap_placement_test b (3, 3))));
  "Checking the failed placement of a building that does not meet the 'on tile' req"
       >:: (fun _ -> assert (((meets_placement_reqs "quarry" (6, 4) j) |> not) 
        && (can_place_building_at "tent" (6, 4) j) |> not));
  "Checking the placement of a building that meets the 'next to tile' placement req" >::
     (fun _ -> let b = place_building "fishing dock" (9, 9) j in 
    assert (matches_test_building (unwrap_placement_test b (9, 9))));
  "Checking the failed placement of a building that doesn't meet the
     'next to tile' placement req" >:: (fun _ -> 
    assert (((meets_placement_reqs "fishing dock" (6, 4) j) |> not) 
        && (can_place_building_at "fishing dock" (6, 4) j) |> not));
  "Checking the failed placement of a building that doesn't type that doesn't exist"
   >:: (fun _ ->  assert ((can_place_building_at "skyscraper" (6, 4) j) |> not));
]

let resource_tests = [
  "Correctly update already existing resource" >:: (fun _ ->
    assert_equal (get_rsc_amt get_test_food_type (update_rsc get_test_food j)) 
    39);
  "Correctly add a non-existing resource" >:: (fun _ ->
    assert_equal 
    ((update_rsc get_test_stone j) |> get_user_resources |> List.length) 6);
  "Correctly update a non-existing resource" >:: (fun _ ->
    assert_equal (get_rsc_amt get_test_stone_type (update_rsc get_test_stone j)) 
    3);
  "Correctly consume resource input and generate resource output"  >:: (fun _ ->
    let st' = (building_consumption_gen (unwrap_test_building j (3, 2)) j) in 
    assert ((get_rsc_amt get_test_wood_type st' ) = 39 && (get_rsc_amt  get_test_planks_type st') = 16));
  "Buiding generates no output when there's insufficient resource input"  >:: (fun _ ->
    let st' = (building_consumption_gen (unwrap_test_building j (3, 1)) j) in 
    assert ((get_rsc_amt get_test_ore_type st') = 2 && (get_rsc_amt get_test_metal_type st') = 6));
  "Building actively generates resource correctly" >:: (fun _ ->
    let st' = (building_active_gen (unwrap_test_building j (6, 1)) j) in 
    assert ((get_rsc_amt get_test_food_type st') = 42 && 
            (st' |> get_user_resources |> List.length) = 5));
]

let population_tests = [
  "Population is 3" >:: (fun _ ->
      (assert_equal (j |> population) 3));
  "List of all residents" >:: (fun _ ->
      assert (cmp_set_like_lists (Gamestate.all_residents j) (all_residents)));
  "List of assigned workers" >:: (fun _ ->
      assert (cmp_set_like_lists (Gamestate.assigned_workers j) (assigned_workers)));
  "List of unassigned workers" >:: (fun _ ->
      assert (cmp_set_like_lists (Gamestate.unassigned_workers j) (unassigned_workers)));
  "Residents of tent 1" >:: (fun _ ->
      assert (cmp_set_like_lists
                (get_building_at (5,6) j |> unwrap_building |> building_residents)
                (tent_1_residents)));
  "Residents of tent 2" >:: (fun _ ->
      assert (cmp_set_like_lists
                (get_building_at (7,5) j |> unwrap_building |> building_residents)
                (tent_2_residents)));
  "Workers at sawmill" >:: (fun _ ->
      assert (cmp_set_like_lists
                (get_building_at (3,2) j |> unwrap_building |> building_workers)
                (sawmill_workers)));
  "Unassign one worker should be 1 worker in building" >:: (fun _ ->
      (assert_equal (unassign_workers_c (3,2) 1 j |> get_building_at (3,2)
                     |> unwrap_building |> building_workers |> List.length) (1)));
  "Assign one worker should be 3 worker in building" >:: (fun _ ->
      (assert_equal (assign_workers_c (3,2) 1 j |> get_building_at (3,2)
                     |> unwrap_building |> building_workers |> List.length) (3)));
  "Assign one worker added the unassigned person's name" >:: (fun _ ->
      assert (cmp_set_like_lists
                (assign_workers_c (3,2) 1 j |> get_building_at (3,2)
                 |> unwrap_building |> building_workers)
                (all_residents)));
  "Unassign two worker and everyone should be unemployed" >:: (fun _ ->
      assert (cmp_set_like_lists
                (unassign_workers_c (3,2) 2 j |> Gamestate.unassigned_workers)
                (all_residents)));
  "Assign workers to non-building raises exn" >:: (fun _ ->
      assert_raises IllegalWorkerAssignment (fun () -> assign_workers_c (1,0) 1 j));
  "Assign workers to tent raises exn" >:: (fun _ ->
      assert_raises IllegalWorkerAssignment (fun () -> assign_workers_c (5,6) 1 j));
  "Assign 2 workers raises exn: only one is available" >:: (fun _ ->
      assert_raises IllegalWorkerAssignment (fun () -> assign_workers_c (3,2) 2 j));
  "Assign -1 workers raises exn" >:: (fun _ ->
      assert_raises IllegalWorkerAssignment (fun () -> assign_workers_c (3,2) ~-1 j));
  "Unassign 3 workers raises exn: only two are available" >:: (fun _ ->
      assert_raises IllegalWorkerAssignment (fun () -> unassign_workers_c (3,2) 3 j));
  "Unassign -1 workers raises exn" >:: (fun _ ->
      assert_raises IllegalWorkerAssignment (fun () -> unassign_workers_c (3,2) ~-1 j));
]

let tests = [
  state_tests;
  building_placement_tests;
  resource_tests;
  population_tests;
]

let suite = "state test suite" >::: List.flatten tests
let _ = run_test_tt_main suite
