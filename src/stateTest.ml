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

(** [unwrap_building b] is the building [b] located at [coor] but not optional *)
let unwrap_building j coor = 
  match (get_building_at coor j) with 
  | Some b -> b
  | None -> failwith "building not found"

(** [unwrap_building b] is the building type [bt] located at [coor] but not optional *)
let unwrap_building_type j coor = 
  match (get_building_type_at coor j) with 
  | Some b -> b
  | None -> failwith "building not found"

(** [matches_test_building b] gives whether b is in the list of valid test buildings *)
let matches_test_building b = 
  match List.find_opt (fun b'-> b = b') get_test_placed_buildings with 
  | Some _ -> true 
  | None -> false 

(** [building_exists j coor] gives whether there is a valid building at [coor]  *)
let building_exists j coor = 
  match (get_building_at coor j) with 
  | Some _ -> true
  | None -> false

(** [unwrap_tile j coor] is the tile type located at [coor] but not optional  *)
let unwrap_tile j coor = 
  match (get_tile_at coor j) with 
  | Some t -> t
  | None -> failwith "tile not found"

let unwrap_building_type_test2 j  = 
  match (get_building_type_at (10, 12) j) with 
  | Some _ -> false
  | None -> true


let tent_1_residents = ["john doe"; "jane doe"]
let tent_2_residents = ["martha"]
let other = [ "sam"; "alex"; "crab"; "drake"]
let all_residents = tent_1_residents @ tent_2_residents @ other
let assigned_workers = ["john doe"; "martha"; "chris hansen"; "sally hansen";]
let unassigned_workers = ["jane doe"; "sam"; "alex"; "crab"; "drake"]
let sawmill_workers = ["john doe"; "martha"]

let state_tests = [
  "Checking get building types" >:: (fun _ ->
      assert (cmp_set_like_lists
                (j |> get_game_data |> GameData.building_types) (building_types)));
  "Checking get resource types" >:: (fun _ ->
      assert (cmp_set_like_lists
                (j |> get_game_data |> GameData.resource_types) (resource_types)));
  "Checking get building at" >:: (fun _ ->
      assert_equal  (unwrap_building j (5,6)) get_test_building);
  "Checking tree farm" >:: (fun _ ->
      assert (building_exists j (6, 4)));
  "Checking get building type at" >:: (fun _ ->
      assert_equal (unwrap_building_type j (6, 4)) "tree farm");
  "Checking get building type at" >:: (fun _ ->
      assert_equal (unwrap_building_type j (3, 2)) "sawmill");
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
        assert (matches_test_building (unwrap_building b (0, 0))));
  "Checking the failed placement of a building on an occupied tile" >:: (fun _ -> 
       assert (((is_empty (7, 5) j) |> not) && (can_place_building_at "tent" (7, 5) j) |> not));
  "Checking the successful placement of a building that meets all resource reqs" >::
     (fun _ -> let b = place_building "quarry" (2, 2) j in 
    assert (matches_test_building (unwrap_building b (2, 2))));
  "Checking the failed placement of a building that does not meet all resource reqs" >::
     (fun _ -> assert (((meets_rsc_reqs "forge" j) |> not) 
            && (can_place_building_at "quarry" (6, 6) j) |> not));
  "Checking the placement of a building that meets the 'on tile' placement req" >::
     (fun _ -> let b = place_building "quarry" (3, 3) j in 
    assert (matches_test_building (unwrap_building b (3, 3))));
  "Checking the failed placement of a building that does not meet the 'on tile' req"
       >:: (fun _ -> assert (((meets_placement_reqs "quarry" (6, 4) j) |> not) 
        && (can_place_building_at "tent" (6, 4) j) |> not));
  "Checking the placement of a building that meets the 'next to tile' placement req" >::
     (fun _ -> let b = place_building "fishing dock" (9, 9) j in 
    assert (matches_test_building (unwrap_building b (9, 9))));
  "Checking the failed placement of a building that doesn't meet the
     'next to tile' placement req" >:: (fun _ -> 
    assert (((meets_placement_reqs "fishing dock" (6, 4) j) |> not) 
        && (can_place_building_at "fishing dock" (6, 4) j) |> not));
  "Checking the failed placement of a building that doesn't type that doesn't exist"
   >:: (fun _ ->  assert ((can_place_building_at "skyscraper" (6, 4) j) |> not));
]

let resource_tests = [
  (* [building_consumption_gen] and [building_active_gen] don't take worker reqs
    into account *)
  "Correctly update already existing resource" >:: (fun _ ->
    assert_equal (get_rsc_amt get_test_food_type (update_rsc get_test_food j)) 
    39);
  "Correctly add a non-existing resource" >:: (fun _ ->
    assert_equal 
    ((update_rsc get_test_stone j) |> get_user_resources |> List.length) 6);
  "Correctly update a non-existing resource" >:: (fun _ ->
    assert_equal (get_rsc_amt get_test_stone_type (update_rsc get_test_stone j)) 
    3);
  "Buiding generates no output when there's insufficient resource input"  >:: (fun _ ->
    let st' = (building_consumption_gen (unwrap_building j (3, 1)) j) in 
    assert ((get_rsc_amt get_test_ore_type st') = 2 && (get_rsc_amt get_test_metal_type st') = 6));
  "Building does not generate resources if there are insufficient workers."  >:: (fun _ ->
    let st' = step j in 
    assert ((get_rsc_amt get_test_food_type st') = 37));
  "Actively generate base output of wood when tree farm has min req workers" >:: (fun _ ->
    let st' = (building_active_gen (unwrap_building j (6, 4)) j) in 
    assert ((get_rsc_amt get_test_wood_type st') = 46));
  "Actively generate correct amt of wood when tree farm has more than
    the req workers" >:: (fun _ ->
    let assigned = (assign_workers_c (6,4) 1 j) in 
    let b = unwrap_building assigned (6,4) in 
    let st' =  building_active_gen b assigned in 
    assert ((get_rsc_amt get_test_wood_type st') = 52));
   "Actively generate correct amt of wood when tree farm has more than
    the req workers and hits max resource" >:: (fun _ ->
    let assigned = (assign_workers_c (6,4) 3 j) in 
    let b = unwrap_building assigned (6,4)  in 
    let st' =  building_active_gen b assigned in 
    assert ((get_rsc_amt get_test_wood_type st') = 54));
  "Actively generate correct amt of wood when tree farm has max 
   req workers" >:: (fun _ ->
    let assigned = (assign_workers_c (6,4) 5 j) in 
    let b = unwrap_building assigned (6,4)  in 
    let st' =  building_active_gen b assigned in 
    assert ((get_rsc_amt get_test_wood_type st') = 54));
 "Correctly consume resource input and generate resource output when sawmill
  has min req workers"  >:: (fun _ ->
    let st' = (building_consumption_gen (unwrap_building j (3, 2)) j) in 
    assert ((get_rsc_amt get_test_wood_type st' ) = 39 &&
    (get_rsc_amt  get_test_planks_type st') = 16));
  "Correctly consume resource input and generate resource output when sawmill
  has more than the req workers"  >:: (fun _ ->
    let assigned = (assign_workers_c (3,2) 1 j) in 
    let b = unwrap_building assigned (3,2)  in 
    let st' =  building_consumption_gen b assigned in 
    assert ((get_rsc_amt get_test_wood_type st' ) = 39 &&
    (get_rsc_amt  get_test_planks_type st') = 20));
  "Correctly consume resource input and generate resource output when sawmill
  has more than the req workers & hits max resource"  >:: (fun _ ->
    let assigned = (assign_workers_c (3,2) 2 j) in 
    let b =  unwrap_building assigned (3,2) in 
    let st' =  building_consumption_gen b assigned in 
    assert ((get_rsc_amt get_test_wood_type st' ) = 39 &&
    (get_rsc_amt  get_test_planks_type st') = 21));
  "Correctly consume resource input and generate resource output when sawmill
  has max workers" >:: (fun _ ->
    let assigned = (assign_workers_c (3,2) 5 j) in 
    let b =  unwrap_building assigned (3,2) in 
    let st' =  building_consumption_gen b assigned in 
    assert ((get_rsc_amt get_test_wood_type st' ) = 39 &&
    (get_rsc_amt  get_test_planks_type st') = 21));
]

let population_tests = [
  "Population is 3" >:: (fun _ ->
      (assert_equal (j |> population) 7));
  "List of all residents" >:: (fun _ ->
      assert (cmp_set_like_lists (Gamestate.all_residents j) (all_residents)));
  "List of assigned workers" >:: (fun _ ->
      assert (cmp_set_like_lists (Gamestate.assigned_workers j) (assigned_workers)));
  "List of unassigned workers" >:: (fun _ ->
      assert (cmp_set_like_lists (Gamestate.unassigned_workers j) (unassigned_workers)));
  "Residents of tent 1" >:: (fun _ ->
      assert (cmp_set_like_lists  
                (unwrap_building j (5,6) |> building_residents)
                (tent_1_residents)));
  "Residents of tent 2" >:: (fun _ ->
      assert (cmp_set_like_lists
                (unwrap_building j (7,5) |> building_residents)
                (tent_2_residents)));
  "Workers at sawmill" >:: (fun _ ->
      assert (cmp_set_like_lists
                (unwrap_building j (3,2) |> building_workers)
                (sawmill_workers)));
  "Unassign one worker should be 1 worker in building" >:: (fun _ ->
      (assert_equal ((unwrap_building (unassign_workers_c (3,2) 1 j) (3, 2))
      |> building_workers |> List.length) (1)));
  "Assign one worker should be 3 worker in building" >:: (fun _ ->
       (assert_equal ((unwrap_building (assign_workers_c (3,2) 1 j) (3, 2))
      |> building_workers |> List.length) (3)));
  "Assign all workers added the unassigned persons' name" >:: (fun _ ->
      assert (cmp_set_like_lists
      ((unwrap_building (assign_workers_c (3,2) 5 j) (3, 2)) |> building_workers)
      (all_residents)));
  "Unassign two worker and everyone should be unemployed" >:: (fun _ ->
      assert (cmp_set_like_lists
        (unassign_workers_c (3,2) 2 j |> Gamestate.unassigned_workers)
        (all_residents)));
  "Assign workers to non-building raises exn" >:: (fun _ ->
      assert_raises IllegalWorkerAssignment (fun () -> assign_workers_c (1,0) 1 j));
  "Assign workers to tent raises exn" >:: (fun _ ->
      assert_raises IllegalWorkerAssignment (fun () -> assign_workers_c (5,6) 1 j));
  "Assign 6 workers raises exn: only five are available" >:: (fun _ ->
      assert_raises IllegalWorkerAssignment (fun () -> assign_workers_c (3,2) 6 j));
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
