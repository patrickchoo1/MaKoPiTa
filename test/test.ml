open OUnit2
open Utility
open Engine.Component
open Engine.System

(**********************************************************************
 * Test Plan
 * Overview: 
 
  Automatic Testing: Sparse Sets, Vector Math, and GJK
  
    * Sprase Sets
      - Tested main functionality (insert, delete, search, clear) through glass
        box, black box, and random methodologies
        - Ex: Inserting edge cases, Inserting random numbers
      - The test cases prove correctness since it covered a wide variety of 
        situations that will be used in the game as well as extreme situations
    
    * Vector Math
      - Conducted black box testing on each of the Vector Math functions
      - One or two unit test cases were implemented per function to test its 
        correctness
      - These test cases will prove the correctness of the vector math 
        functions, allowing Vector Math to be implemented elsewhere in the code
    
    * GJK Collision
      - Conducted black box testing on the GJK algorithm
      - Tested the collision between Points, Circles, and Polygons 
      - Primarily used Points to test edge cases (border, inside, and outside of
        shape)
      - Further tested Circles/Polygons collisions with each other
      - Used basic Circles and Polygons, and a random (weird) Polygon to test
      - These test cases will prove correctness of the GJK algorithm for we test
        all combinations possible collisisions between two shapes
      
  Manual Testing: Rendering, Timing, Scene Transitions, Score & Health Tracking,
    and Audio
  
     - Tested game mechanics
     - Compared game timer to external stopwatch
     - Physically tested gui from various condition for collision detection,
       and score & health tracking 
     - Gameplay throughs with gameovers and wins
     - Tried all permutations of scenes paths
     - General print debugging
     - Large combination of blackbox, glassbox, and random testing for all
     - Proves correctness since we covered all possibilites that user can do
       during game 
  **********************************************************************)

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
    sparse_search_test "Search for 1 in set1" set1 1 0;
    sparse_search_test "Search for 2 in set1" set1 2 1;
    sparse_search_test "Search for 3 in set1" set1 3 2;
    sparse_search_test "Search for 4 in set1" set1 4 3;
    sparse_search_test "Search for 0 in set1" set1 0 4;
    sparse_search_test "Search for 5 in set1" set1 5 5;
    sparse_search_test "Search for 7 in set1" set1 7 6;
    sparse_search_test "Search for element not in set1" set1 6 ~-1;
    sparse_set_to_list_test "List of set1" set1
      [ 0; 1; 2; 3; 4; 5; 7; 10; 10000 ];
    sparse_search_test "Search for 3 removed from set2" set2 3 ~-1;
    sparse_search_test "Search for 5 removed from set2" set2 5 ~-1;
    sparse_set_to_list_test "List of set2" set2 [ 0; 1; 2; 4; 7; 10; 10000 ];
    sparse_set_to_list_test "Cleared list of set3" set3 [];
  ]

(**********************************************************************
 * Vector Math Tests
 **********************************************************************)

(** [vec_to_string v] convert a vector [v] to a string in the form ["x, y, z"] *)
let vec_to_string (v : Vector.s) =
  match v.vec with
  | x, y, z ->
      string_of_float x ^ ", " ^ string_of_float y ^ ", " ^ string_of_float z

let vector_math_tests =
  [
    ( "zero_vec is origin" >:: fun _ ->
      assert_equal { Vector.vec = (0., 0., 0.) } VectorMath.zero_vec );
    ( "make_vec 1 1 1 is (1, 1, 1)" >:: fun _ ->
      assert_equal { Vector.vec = (1., 1., 1.) } (VectorMath.make_vec 1. 1. 1.)
    );
    ( "(0, 0, 0) + (1, 1, 1) is (1, 1, 1)" >:: fun _ ->
      assert_equal
        { Vector.vec = (1., 1., 1.) }
        (VectorMath.add VectorMath.zero_vec (VectorMath.make_vec 1. 1. 1.)) );
    ( "(1, 1, 1) + (1, 1, 1) is (2, 2, 2)" >:: fun _ ->
      assert_equal
        { Vector.vec = (2., 2., 2.) }
        (VectorMath.add
           (VectorMath.make_vec 1. 1. 1.)
           (VectorMath.make_vec 1. 1. 1.)) );
    ( "(0, 0, 0) - (1, 1, 1) is (-1, -1, -1)" >:: fun _ ->
      assert_equal
        { Vector.vec = (~-.1., ~-.1., ~-.1.) }
        (VectorMath.sub VectorMath.zero_vec (VectorMath.make_vec 1. 1. 1.)) );
    ( "(2, 1, 0) - (1, 1, 1) is (1, 0, -1)" >:: fun _ ->
      assert_equal
        { Vector.vec = (1., 0., ~-.1.) }
        (VectorMath.sub
           (VectorMath.make_vec 2. 1. 0.)
           (VectorMath.make_vec 1. 1. 1.)) );
    ( "-(0, 0, 0) is (0, 0, 0)" >:: fun _ ->
      assert_equal VectorMath.zero_vec (VectorMath.neg VectorMath.zero_vec) );
    ( "-(1, 1, 1) is (-1, -1, -1)" >:: fun _ ->
      assert_equal
        { Vector.vec = (~-.1., ~-.1., ~-.1.) }
        (VectorMath.neg (VectorMath.make_vec 1. 1. 1.)) );
    ( "(0, 0, 0)*2 is (0, 0, 0)" >:: fun _ ->
      assert_equal VectorMath.zero_vec (VectorMath.scale VectorMath.zero_vec 2.)
    );
    ( "(1, 1, 1)*2 is (2, 2, 2)" >:: fun _ ->
      assert_equal
        { Vector.vec = (2., 2., 2.) }
        (VectorMath.scale (VectorMath.make_vec 1. 1. 1.) 2.) );
    ( "(0, 0, 0) * (1, 1, 1) is 0" >:: fun _ ->
      assert_equal 0.
        (VectorMath.dot VectorMath.zero_vec (VectorMath.make_vec 1. 1. 1.)) );
    ( "(1, 2, 3) * (3, 2, 1) is 10" >:: fun _ ->
      assert_equal 10.
        (VectorMath.dot
           (VectorMath.make_vec 1. 2. 3.)
           (VectorMath.make_vec 3. 2. 1.)) );
    ( "(0, 0, 0) x (1, 1, 1) is (0, 0, 0)" >:: fun _ ->
      assert_equal VectorMath.zero_vec
        (VectorMath.cross VectorMath.zero_vec (VectorMath.make_vec 1. 1. 1.)) );
    ( "(1, 2, 3) * (3, 2, 1) is (-4, 8, -4)" >:: fun _ ->
      assert_equal
        { Vector.vec = (~-.4., 8., ~-.4.) }
        (VectorMath.cross
           (VectorMath.make_vec 1. 2. 3.)
           (VectorMath.make_vec 3. 2. 1.)) );
    ( "(3, 2, 1) * (1, 2, 3) is (4, -8, 4)" >:: fun _ ->
      assert_equal
        { Vector.vec = (4., ~-.8., 4.) }
        (VectorMath.cross
           (VectorMath.make_vec 3. 2. 1.)
           (VectorMath.make_vec 1. 2. 3.)) );
    ( "|(0, 0, 0)| is 0" >:: fun _ ->
      assert_equal 0. (VectorMath.magnitude VectorMath.zero_vec) );
    ( "|(2, 1, 2)| is 3" >:: fun _ ->
      assert_equal 3. (VectorMath.magnitude (VectorMath.make_vec 2. 1. 2.)) );
    ( "|(3, 4, 0)| is 5" >:: fun _ ->
      assert_equal 5. (VectorMath.magnitude (VectorMath.make_vec 3. 4. 0.)) );
  ]

(**********************************************************************
 * GJK Tester Functions
 **********************************************************************)

let gjk_test (name : string) (target : Shape.s) (pos : Shape.s)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (GJK.gjk_collision target pos)
    ~printer:string_of_bool

let pos_x : Vector.s = VectorMath.make_vec 1. 0. 0.
let neg_x : Vector.s = VectorMath.make_vec ~-.1. 0. 0.
let pos_y : Vector.s = VectorMath.make_vec 0. 1. 0.
let neg_y : Vector.s = VectorMath.make_vec 0. ~-.1. 0.

let random_vert : Vector.s list =
  [
    VectorMath.make_vec 5. 5. 0.;
    VectorMath.make_vec 100. 5. 0.;
    VectorMath.make_vec 700. 300. 0.;
    VectorMath.make_vec 300. 500. 0.;
    VectorMath.make_vec 5. 100. 0.;
  ]

let unit_circle : Shape.s =
  Circle { radius = 1.0; center = VectorMath.zero_vec }

let triangle : Shape.s = Polygon { verticies = [ neg_x; pos_x; pos_y ] }
let quad : Shape.s = Polygon { verticies = [ pos_x; pos_y; neg_x; neg_y ] }
let random_shape : Shape.s = Polygon { verticies = random_vert }
let pos00 : Shape.s = Point { center = VectorMath.zero_vec }
let pos10 : Shape.s = Point { center = pos_x }
let pos01 : Shape.s = Point { center = pos_y }
let pos_10 : Shape.s = Point { center = neg_x }
let pos0_1 : Shape.s = Point { center = neg_y }
let pos11 : Shape.s = Point { center = VectorMath.add pos_x pos_y }
let pos_11 : Shape.s = Point { center = VectorMath.add neg_x pos_y }
let pos_1_1 : Shape.s = Point { center = VectorMath.add neg_x neg_y }
let pos1_1 : Shape.s = Point { center = VectorMath.add pos_x neg_y }

let gjk_tests =
  [
    gjk_test "(0,0) is in unit circle" unit_circle pos00 true;
    gjk_test "(1,0) is in unit circle" unit_circle pos10 true;
    gjk_test "(0,1) is in unit circle" unit_circle pos01 true;
    gjk_test "(0,-1) is in unit circle" unit_circle pos0_1 true;
    gjk_test "(1,1) is not in unit circle" unit_circle pos11 false;
    gjk_test "(-1,1) is not in unit circle" unit_circle pos_11 false;
    gjk_test "(-1,-1) is not in unit circle" unit_circle pos_1_1 false;
    gjk_test "(1,-1) is not in unit circle" unit_circle pos1_1 false;
    gjk_test "(0,0) is in triangle" triangle pos00 true;
    gjk_test "(1,0) is in triangle" triangle pos10 true;
    gjk_test "(0,1) is in triangle" triangle pos01 true;
    gjk_test "(-1,0) is in triangle" triangle pos_10 true;
    gjk_test "(0,-1) is not in triangle" triangle pos0_1 false;
    gjk_test "(1,1) is not in triangle" triangle pos11 false;
    gjk_test "(-1,1) is not in triangle" triangle pos_11 false;
    gjk_test "(0,0) is in quadrilateral" quad pos00 true;
    gjk_test "(1,0) is in quadrilateral" quad pos10 true;
    gjk_test "(0,1) is in quadrilateral" quad pos01 true;
    gjk_test "(-1,0) is in quadrilateral" quad pos_10 true;
    gjk_test "(0,-1) is in quadrilateral" quad pos0_1 true;
    gjk_test "(1,1) is not in quadrilateral" quad pos11 false;
    gjk_test "(-1,1) is not in quadrilateral" quad pos_11 false;
    gjk_test "(-1,-1) is not in quadrilateral" quad pos_1_1 false;
    gjk_test "(1,-1) is not in quadrilateral" quad pos1_1 false;
    gjk_test "(5,5) is in random shape" random_shape
      (Point { center = VectorMath.make_vec 5. 5. 0. })
      true;
    gjk_test "(100,5) is in random shape" random_shape
      (Point { center = VectorMath.make_vec 100. 5. 0. })
      true;
    gjk_test "(700,300) is in random shape" random_shape
      (Point { center = VectorMath.make_vec 700. 300. 0. })
      true;
    gjk_test "(300,500) is in random shape" random_shape
      (Point { center = VectorMath.make_vec 300. 500. 0. })
      true;
    gjk_test "(5,100) is in random shape" random_shape
      (Point { center = VectorMath.make_vec 5. 100. 0. })
      true;
    gjk_test "(300,300) is in random shape" random_shape
      (Point { center = VectorMath.make_vec 300. 300. 0. })
      true;
    gjk_test "(0,0) is not in random shape" random_shape pos00 false;
    gjk_test "(600,400) is not in random shape" random_shape
      (Point { center = VectorMath.make_vec 600. 400. 0. })
      false;
    gjk_test "(500,180) is not in random shape" random_shape
      (Point { center = VectorMath.make_vec 500. 180. 0. })
      false;
    gjk_test "(200,400) is not in random shape" random_shape
      (Point { center = VectorMath.make_vec 200. 400. 0. })
      false;
    gjk_test "circle is in triangle" unit_circle triangle true;
    gjk_test "circle is in quad" unit_circle triangle true;
    gjk_test "circle is not in random shape" unit_circle random_shape false;
    gjk_test "triangle is in circle" triangle unit_circle true;
    gjk_test "triangle is in quad" triangle quad true;
    gjk_test "triangle is not in random shape" triangle random_shape false;
    gjk_test "quad is in circle" quad unit_circle true;
    gjk_test "quad is in triangle" quad triangle true;
    gjk_test "quad is not in random shape" quad random_shape false;
  ]

let suite =
  "search test suite"
  >::: List.flatten [ sparse_arraylist_tests; vector_math_tests; gjk_tests ]

let _ = run_test_tt_main suite
