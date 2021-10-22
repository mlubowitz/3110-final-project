open OUnit2
open Chess
open Board
open Pieces

let is_legal_test name ori_loc new_loc expected =
  name >:: fun _ -> assert_equal expected (is_legal ori_loc new_loc)

let is_legal_tests = []

let pieces_tests = []

(* ===============board tests below================================= *)
let get_str_piece_test name board grid expected =
  name >:: fun _ -> assert_equal expected (get_str_piece board grid)

let get_row_test name board row_num expected =
  name >:: fun _ -> assert_equal expected (get_row board row_num)

let test_board = init_board ()

let board_tests =
  [
    get_str_piece_test "test row 0" test_board (0, 0) "[ ]";
    get_str_piece_test "test row 1" test_board (1, 5) "[P]";
    get_str_piece_test "test row 1" test_board (1, 2) "[P]";
    get_str_piece_test "test row 2" test_board (2, 5) "[ ]";
    get_str_piece_test "test row 3" test_board (3, 5) "[ ]";
    get_str_piece_test "test row 4" test_board (4, 5) "[ ]";
    get_str_piece_test "test row 4" test_board (4, 0) "[ ]";
    get_str_piece_test "test row 5" test_board (5, 3) "[ ]";
    get_str_piece_test "test row 6" test_board (6, 7) "[p]";
    get_str_piece_test "test row 6" test_board (6, 6) "[p]";
    get_str_piece_test "test row 7" test_board (7, 7) "[ ]";
    (* Violate encapsulation? Should not know what strings look like?
       But it is printed out in our terminal representation *)
    get_row_test "test row 7" test_board 7
      [ "[ ]"; "[ ]"; "[ ]"; "[ ]"; "[ ]"; "[ ]"; "[ ]"; "[ ]" ];
    get_row_test "test row 6" test_board 6
      [ "[p]"; "[p]"; "[p]"; "[p]"; "[p]"; "[p]"; "[p]"; "[p]" ];
    get_row_test "test row 1" test_board 1
      [ "[P]"; "[P]"; "[P]"; "[P]"; "[P]"; "[P]"; "[P]"; "[P]" ];
  ]

let tests =
  "test suite for Chess" >::: List.flatten [ pieces_tests; board_tests ]

let _ = run_test_tt_main tests