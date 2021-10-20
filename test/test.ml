open OUnit2
open Chess
open Board
open Pieces

(* let piece_location_test name piece expected = name >:: fun _ ->
   assert_equal expected (piece_location piece) *)

let is_legal_test name ori_loc new_loc expected =
  name >:: fun _ -> assert_equal expected (is_legal ori_loc new_loc)

let is_legal_tests = []

let pieces_tests = []

let board_tests = []

let tests =
  "test suite for Chess" >::: List.flatten [ pieces_tests; board_tests ]

let _ = run_test_tt_main tests