open OUnit2
open Chess
open Board
open Pieces
open State

let is_legal_test name ori_piece new_piece expected =
  name >:: fun _ -> assert_equal expected (is_legal ori_piece new_piece)

let empty_1_1 = to_piece (1, 1) "[ ]"

let empty_2_1 = to_piece (2, 1) "[ ]"

let empty_1_7 = to_piece (1, 7) "[ ]"

let empty_6_6 = to_piece (6, 6) "[ ]"

let empty_7_1 = to_piece (7, 1) "[ ]"

let empty_6_1 = to_piece (6, 1) "[ ]"

let bishop_4_4 = to_piece (4, 4) "[b]"

let bishop_4_4_diff_color = to_piece (4, 4) "[B]"

let bishop_0_0 = to_piece (0, 0) "[b]"

let rook_1_0 = to_piece (1, 0) "[r]"

let rook_5_1 = to_piece (5, 1) "[r]"

let rook_0_1 = to_piece (0, 1) "[r]"

let rook_1_4 = to_piece (1, 4) "[r]"

let pawn_3_1_no_first_move = to_piece (3, 1) "[p]"

let pawn_3_1_first_move = first_move (to_piece (3, 1) "[p]")

let king_5_6 = to_piece (5, 6) "[k]"

let king_5_5 = to_piece (5, 5) "[k]"

let king_6_5 = to_piece (6, 5) "[k]"

let king_7_5 = to_piece (7, 5) "[k]"

let king_7_6 = to_piece (7, 6) "[k]"

let king_7_7 = to_piece (7, 7) "[k]"

let king_6_7 = to_piece (6, 7) "[k]"

let king_5_7 = to_piece (5, 7) "[k]"

let knight_4_5 = to_piece (4, 5) "[n]"

let knight_5_4 = to_piece (5, 4) "[n]"

let knight_7_4 = to_piece (7, 4) "[n]"

let knight_4_7 = to_piece (4, 7) "[n]"

let knight_3_2 = to_piece (3, 2) "[n]"

let knight_0_3 = to_piece (0, 3) "[n]"

let knight_3_0 = to_piece (3, 0) "[n]"

let knight_2_3 = to_piece (2, 3) "[n]"

let is_legal_tests =
  [
    is_legal_test "bishop from 4,4 to 1,1 is true" bishop_4_4 empty_1_1
      true;
    is_legal_test "bishop from 4,4 to 1,7 is true" bishop_4_4 empty_1_7
      true;
    is_legal_test "bishop from 4,4 to 6,6 is true" bishop_4_4 empty_6_6
      true;
    is_legal_test "bishop from 4,4 to 7,1 is true" bishop_4_4 empty_7_1
      true;
    is_legal_test "bishop from 4,4 to 6,1 is false" bishop_4_4 empty_6_1
      false;
    is_legal_test "rook from 1,0 to 1,1 is true" rook_1_0 empty_1_1 true;
    is_legal_test "rook from 5,1 to 1,1 is true" rook_5_1 empty_1_1 true;
    is_legal_test "rook from 0,1 to 1,1 is true" rook_0_1 empty_1_1 true;
    is_legal_test "rook from 1,4 to 1,1 is true" rook_1_4 empty_1_1 true;
    is_legal_test "rook from 1,4 to 6,6 is false" rook_1_4 empty_6_6
      false;
    is_legal_test "rook from 1,4 to same color bishop on 4,4 is false"
      rook_1_4 bishop_4_4 false;
    is_legal_test
      "rook from 1,4 to different color bishop on 4,4 is true" rook_1_4
      bishop_4_4_diff_color true;
    is_legal_test "pawn from 3,1 to 1,1 is true" pawn_3_1_no_first_move
      empty_1_1 true;
    is_legal_test "pawn from 3,1 to 2,1 is true" pawn_3_1_no_first_move
      empty_2_1 true;
    is_legal_test "pawn from 3,1 to 1,7 is false" pawn_3_1_no_first_move
      empty_1_7 false;
    is_legal_test "pawn after moving from 3,1 to 1,1 is false"
      pawn_3_1_first_move empty_1_1 false;
    is_legal_test "king from 5,6 to 6,6 is true" king_5_6 empty_6_6 true;
    is_legal_test "king from 5,5 to 6,6 is true" king_5_5 empty_6_6 true;
    is_legal_test "king from 6,5 to 6,6 is true" king_6_5 empty_6_6 true;
    is_legal_test "king from 7,5 to 6,6 is true" king_7_5 empty_6_6 true;
    is_legal_test "king from 7,6 to 6,6 is true" king_7_6 empty_6_6 true;
    is_legal_test "king from 7,7 to 6,6 is true" king_7_7 empty_6_6 true;
    is_legal_test "king from 6,7 to 6,6 is true" king_6_7 empty_6_6 true;
    is_legal_test "king from 5,7 to 6,6 is true" king_5_7 empty_6_6 true;
    is_legal_test "king from 5,6 to 7,1 is false" king_5_6 empty_7_1
      false;
    is_legal_test "knight from 4,5 to 6,6 is true" knight_4_5 empty_6_6
      true;
    is_legal_test "knight from 5,4 to 6,6 is true" knight_5_4 empty_6_6
      true;
    is_legal_test "knight from 7,4 to 6,6 is true" knight_7_4 empty_6_6
      true;
    is_legal_test "knight from 4,7 to 6,6 is true" knight_4_7 empty_6_6
      true;
    is_legal_test "knight from 3,2 to 1,1 is true" knight_3_2 empty_1_1
      true;
    is_legal_test "knight from 0,3 to 1,1 is true" knight_0_3 empty_1_1
      true;
    is_legal_test "knight from 3,0 to 1,1 is true" knight_3_0 empty_1_1
      true;
    is_legal_test "knight from 2,3 to 1,1 is true" knight_2_3 empty_1_1
      true;
    is_legal_test "knight from 2,3 to 6,6 is falses" knight_2_3
      empty_6_6 false;
  ]

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

let in_check_test name state piece expected =
  name >:: fun _ ->
  assert_equal expected (in_check state piece) ~printer:string_of_bool

(* Get the state of the board we created above (board with pieces in
   initial layout) *)
let test_st = init_state test_board

let a = move_piece test_board (6, 2) (5, 2)

let b = move_piece a (6, 4) (5, 4)

let in_check_board = move_piece b (7, 3) (4, 0)

let in_check_test_st = init_state in_check_board

let checked_king = what_piece in_check_test_st (7, 3)

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

let in_check_tests =
  [
    in_check_test "diagonal check from queen" in_check_test_st
      checked_king true;
  ]

let tests =
  "test suite for Chess"
  >::: List.flatten
         [
           is_legal_tests;
           pieces_tests;
           board_tests;
           state_tests;
           in_check_tests;
         ]

let _ = run_test_tt_main tests