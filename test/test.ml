open OUnit2
open Chess
open Board

let piece_location_test name piece expected = 
  name >:: (fun _ -> assert_equal expected (piece_location piece))

let pieces_tests=[]
let board_tests=[piece_location_test "test1" p1 (0,0)]
let tests =
  "test suite for Chess"
  >::: List.flatten [ pieces_tests; board_tests ]

let _ = run_test_tt_main tests