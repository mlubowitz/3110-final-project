open OUnit2
open Chess
open Board
open Pieces
open State

let is_legal_test name ori_loc new_loc expected =
  name >:: fun _ -> assert_equal expected (is_legal ori_loc new_loc)

let is_legal_tests = []

let pieces_tests = []

(* ===============board tests below================================= *)
let get_str_piece_test name board grid expected =
  name >:: fun _ -> assert_equal expected (get_str_piece board grid)

let get_row_test name board row_num expected =
  name >:: fun _ -> assert_equal expected (get_row board row_num)

(*A test board. It is the initial chess board with nothing moved yet. *)
let test_board = init_board ()

let board_tests =
  [
    (* I know test names are not very descriptive right now. *)
    get_str_piece_test "test row 0" test_board (0, 0) "[R]";
    get_str_piece_test "test row 1" test_board (1, 5) "[P]";
    get_str_piece_test "test row 1" test_board (1, 2) "[P]";
    get_str_piece_test "test row 2" test_board (2, 5) "[ ]";
    get_str_piece_test "test row 3" test_board (3, 5) "[ ]";
    get_str_piece_test "test row 4" test_board (4, 5) "[ ]";
    get_str_piece_test "test row 4" test_board (4, 0) "[ ]";
    get_str_piece_test "test row 5" test_board (5, 3) "[ ]";
    get_str_piece_test "test row 6" test_board (6, 7) "[p]";
    get_str_piece_test "test row 6" test_board (6, 6) "[p]";
    get_str_piece_test "test row 7" test_board (7, 7) "[r]";
    (* Violate encapsulation? Should not know what strings look like?
       But it is printed out in our terminal representation *)
    get_row_test "get list of pieces in row 7" test_board 7
      [ "[r]"; "[n]"; "[b]"; "[q]"; "[k]"; "[b]"; "[n]"; "[r]" ];
    get_row_test "get list of pieces in row 6" test_board 6
      [ "[p]"; "[p]"; "[p]"; "[p]"; "[p]"; "[p]"; "[p]"; "[p]" ];
    get_row_test "get list of pieces in row 1" test_board 1
      [ "[P]"; "[P]"; "[P]"; "[P]"; "[P]"; "[P]"; "[P]"; "[P]" ];
    get_row_test "get list of pieces in row 0" test_board 0
      [ "[R]"; "[N]"; "[B]"; "[Q]"; "[K]"; "[B]"; "[N]"; "[R]" ];
  ]

(* ===============state tests below================================= *)
let is_path_empty_test name state ori_loc new_loc expected =
  name >:: fun _ ->
  assert_equal expected
    (is_path_empty state ori_loc new_loc)
    ~printer:string_of_bool

let find_king_test name state color expected =
  name >:: fun _ -> assert_equal expected (find_king state color)

(* Get the state of the board we created above (board with pieces in
   initial layout) *)
let test_st = init_state test_board

let state_tests =
  [
    is_path_empty_test "vert-nothing in btwn-going up" test_st (5, 7)
      (3, 7) true;
    is_path_empty_test "vert-nothing in btwn-going down" test_st (1, 5)
      (4, 5) true;
    is_path_empty_test "vert-piece in btwn-going up" test_st (7, 7)
      (3, 7) false;
    is_path_empty_test "vert-piece in btwn-going down" test_st (0, 3)
      (4, 3) false;
    is_path_empty_test "hori-piece in btwn-going left" test_st (7, 7)
      (7, 5) false;
    is_path_empty_test "hori-piece in btwn-going right" test_st (1, 1)
      (1, 5) false;
    is_path_empty_test "hori-nothing in btwn-going right" test_st (5, 2)
      (5, 6) true;
    is_path_empty_test "hori-nothing in btwn-going left" test_st (5, 5)
      (5, 0) true;
    is_path_empty_test "dia-nothing in btwn-going NE" test_st (6, 1)
      (3, 4) true;
    is_path_empty_test "dia-nothing in btwn-going NW" test_st (6, 7)
      (5, 6) true;
    is_path_empty_test "dia-nothing in btwn-going SE" test_st (1, 2)
      (3, 4) true;
    is_path_empty_test "dia-nothing in btwn-going SW" test_st (1, 2)
      (3, 1) true;
    is_path_empty_test "dia-piece in btwn-going NE" test_st (7, 0)
      (2, 5) false;
    is_path_empty_test "dia-piece in btwn-going NW" test_st (7, 5)
      (5, 3) false;
    is_path_empty_test "dia-piece in btwn-going SW" test_st (0, 7)
      (5, 2) false;
    is_path_empty_test "dia-piece in btwn-going SE" test_st (0, 2)
      (3, 5) false;
    find_king_test "loc of black king in init board should be (0,4)"
      test_st "B" (0, 4);
    find_king_test "loc of white king in init board should be (7,4)"
      test_st "W" (7, 4);
  ]

let tests =
  "test suite for Chess"
  >::: List.flatten [ pieces_tests; board_tests; state_tests ]

let _ = run_test_tt_main tests