open OUnit2
open Utility

(**********************************************************************
 * Pretty Printer Helper Functions
 **********************************************************************)

let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

(**********************************************************************
 * Pretty Printer Helper Functions
 **********************************************************************)

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether they are
    equivalent set-like lists. That means checking two things. First, they must
    both be "set-like", meaning that they do not contain any duplicates. Second,
    they must contain the same elements, though not necessarily in the same
    order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

(**********************************************************************
 * Sparse Arraylist Tester Functions
 **********************************************************************)

let sparse_search_test (name : string) (s : Sparse_arraylist.t) (id : int)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Sparse_arraylist.search id s)
    ~printer:string_of_int

let sparse_set_to_list_test (name : string) (s : Sparse_arraylist.t)
    (expected_output : int list) : test =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list string_of_int)
    expected_output
    (Sparse_arraylist.set_to_list s)

let set1 =
  Sparse_arraylist.make 5 |> Sparse_arraylist.insert 1
  |> Sparse_arraylist.insert 2 |> Sparse_arraylist.insert 3
  |> Sparse_arraylist.insert 4 |> Sparse_arraylist.insert 0
  |> Sparse_arraylist.insert 5 |> Sparse_arraylist.insert 2
  |> Sparse_arraylist.insert 7
  |> Sparse_arraylist.insert 10000
  |> Sparse_arraylist.insert 10

let set2 =
  Sparse_arraylist.make 5 |> Sparse_arraylist.insert 1
  |> Sparse_arraylist.insert 2 |> Sparse_arraylist.insert 3
  |> Sparse_arraylist.insert 4 |> Sparse_arraylist.insert 0
  |> Sparse_arraylist.insert 5 |> Sparse_arraylist.insert 7
  |> Sparse_arraylist.insert 10000
  |> Sparse_arraylist.insert 10 |> Sparse_arraylist.delete 3
  |> Sparse_arraylist.delete 5 |> Sparse_arraylist.delete 11

let set3 =
  Sparse_arraylist.make 5 |> Sparse_arraylist.insert 1
  |> Sparse_arraylist.insert 2 |> Sparse_arraylist.insert 3
  |> Sparse_arraylist.insert 4 |> Sparse_arraylist.insert 0
  |> Sparse_arraylist.insert 5 |> Sparse_arraylist.insert 7
  |> Sparse_arraylist.insert 10000
  |> Sparse_arraylist.insert 10 |> Sparse_arraylist.clear

let sparse_arraylist_tests =
  [
    sparse_search_test "Search for element in set1" set1 1 0;
    sparse_search_test "Search for element in set1" set1 2 1;
    sparse_search_test "Search for element in set1" set1 3 2;
    sparse_search_test "Search for element in set1" set1 4 3;
    sparse_search_test "Search for element in set1" set1 0 4;
    sparse_search_test "Search for element in set1" set1 5 5;
    sparse_search_test "Search for element in set1" set1 7 6;
    sparse_search_test "Search for element not in set1" set1 6 ~-1;
    sparse_set_to_list_test "List of set1" set1
      [ 0; 1; 2; 3; 4; 5; 7; 10; 10000 ];
    sparse_search_test "Search for element removed from set2" set2 3 ~-1;
    sparse_search_test "Search for element removed from set2" set2 5 ~-1;
    sparse_set_to_list_test "List of set1" set2 [ 0; 1; 2; 4; 7; 10; 10000 ];
    sparse_set_to_list_test "Cleared list of set3" set3 [];
  ]

let suite = "search test suite" >::: List.flatten [ sparse_arraylist_tests ]
let _ = run_test_tt_main suite
