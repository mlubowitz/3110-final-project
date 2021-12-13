open OUnit2
open Chess
open Board
open Pieces
open State
(************************************************************************
  Our testing strategy:

  CURRENT FUNCTIONS WITH TEST CASES IN THIS FILE:

  [in_check]

  [piece_in_path]

  [knight_check_piece]

  [orthog_adj_check_piece]

  [diag_check_piece]

  [st_with_two_pces]

  [grd] THIS IS IMPLEMENTED IN THIS FILE TO HELP WITH TESTING

  [is_path_empty]

  [find_king]

  [get_str_piece]

  [get_row]

  [is_legal_PIECES]

  [is_legal_castle]

  What we tested with OUnit vs manual:

  ***************************************************************************)

(* Pieces to use for testing. *)
(* empty pieces *)
let empty_1_1 = to_piece (1, 1) "[ ]"

let empty_2_1 = to_piece (2, 1) "[ ]"

let empty_1_7 = to_piece (1, 7) "[ ]"

let empty_6_6 = to_piece (6, 6) "[ ]"

let empty_7_1 = to_piece (7, 1) "[ ]"

let empty_6_1 = to_piece (6, 1) "[ ]"

(* queens *)
let w_queen_7_3 = to_piece (7, 3) "[q]"

let b_queen_0_3 = to_piece (0, 3) "[Q]" (*BLK*)

(* bishops*)
let w_bishop_4_4 = to_piece (4, 4) "[b]"

let b_bishop_4_4 = to_piece (4, 4) "[B]" (*BLK*)

let w_bishop_0_0 = to_piece (0, 0) "[b]"

(* rooks *)
let w_rook_1_0 = to_piece (1, 0) "[r]"

let w_rook_5_1 = to_piece (5, 1) "[r]"

let w_rook_0_1 = to_piece (0, 1) "[r]"

let w_rook_1_4 = to_piece (1, 4) "[r]"

let b_rook_0_0 = to_piece (0, 0) "[R]" (*BLK*)

(* pawns *)
let b_pawn = to_piece (3, 1) "[P]" (*BLK*)

let w_pawn = to_piece (0, 1) "[p]"

let w_pawn_3_1_no_first_move = to_piece (3, 1) "[p]"

let w_pawn_3_1_first_move = first_move (to_piece (3, 1) "[p]")

(* kings *)
let b_king_0_4 = to_piece (0, 4) "[K]" (*BLK*)

let w_king_5_6 = to_piece (5, 6) "[k]"

let w_king_5_5 = to_piece (5, 5) "[k]"

let w_king_6_5 = to_piece (6, 5) "[k]"

let w_king_7_5 = to_piece (7, 5) "[k]"

let w_king_7_6 = to_piece (7, 6) "[k]"

let w_king_7_7 = to_piece (7, 7) "[k]"

let w_king_6_7 = to_piece (6, 7) "[k]"

let w_king_5_7 = to_piece (5, 7) "[k]"

(* knights *)
let w_knight_4_5 = to_piece (4, 5) "[n]"

let b_knight_4_2 = to_piece (4, 2) "[N]" (*BLK*)

let w_knight_5_4 = to_piece (5, 4) "[n]"

let w_knight_7_4 = to_piece (7, 4) "[n]"

let w_knight_4_7 = to_piece (4, 7) "[n]"

let w_knight_3_2 = to_piece (3, 2) "[n]"

let w_knight_0_3 = to_piece (0, 3) "[n]"

let w_knight_3_0 = to_piece (3, 0) "[n]"

let w_knight_2_3 = to_piece (2, 3) "[n]"

let is_legal_PIECES_test name ori_piece new_piece expected =
  name >:: fun _ ->
  assert_equal expected (is_legal_PIECES ori_piece new_piece)

let is_legal_PIECES_tests =
  [
    is_legal_PIECES_test "bishop from 4,4 to 1,1 is true" w_bishop_4_4
      empty_1_1 true;
    is_legal_PIECES_test "bishop from 4,4 to 1,7 is true" w_bishop_4_4
      empty_1_7 true;
    is_legal_PIECES_test "bishop from 4,4 to 6,6 is true" w_bishop_4_4
      empty_6_6 true;
    is_legal_PIECES_test "bishop from 4,4 to 7,1 is true" w_bishop_4_4
      empty_7_1 true;
    is_legal_PIECES_test "bishop from 4,4 to 6,1 is false" w_bishop_4_4
      empty_6_1 false;
    is_legal_PIECES_test "rook from 1,0 to 1,1 is true" w_rook_1_0
      empty_1_1 true;
    is_legal_PIECES_test "rook from 5,1 to 1,1 is true" w_rook_5_1
      empty_1_1 true;
    is_legal_PIECES_test "rook from 0,1 to 1,1 is true" w_rook_0_1
      empty_1_1 true;
    is_legal_PIECES_test "rook from 1,4 to 1,1 is true" w_rook_1_4
      empty_1_1 true;
    is_legal_PIECES_test "rook from 1,4 to 6,6 is false" w_rook_1_4
      empty_6_6 false;
    is_legal_PIECES_test
      "rook from 1,4 to same color bishop on 4,4 is false" w_rook_1_4
      w_bishop_4_4 false;
    is_legal_PIECES_test
      "rook from 1,4 to different color bishop on 4,4 is true"
      w_rook_1_4 b_bishop_4_4 true;
    is_legal_PIECES_test "pawn from 3,1 to 1,1 is true"
      w_pawn_3_1_no_first_move empty_1_1 true;
    is_legal_PIECES_test "pawn from 3,1 to 2,1 is true"
      w_pawn_3_1_no_first_move empty_2_1 true;
    is_legal_PIECES_test "pawn from 3,1 to 1,7 is false"
      w_pawn_3_1_no_first_move empty_1_7 false;
    is_legal_PIECES_test "pawn after moving from 3,1 to 1,1 is false"
      w_pawn_3_1_first_move empty_1_1 false;
    is_legal_PIECES_test "king from 5,6 to 6,6 is true" w_king_5_6
      empty_6_6 true;
    is_legal_PIECES_test "king from 5,5 to 6,6 is true" w_king_5_5
      empty_6_6 true;
    is_legal_PIECES_test "king from 6,5 to 6,6 is true" w_king_6_5
      empty_6_6 true;
    is_legal_PIECES_test "king from 7,5 to 6,6 is true" w_king_7_5
      empty_6_6 true;
    is_legal_PIECES_test "king from 7,6 to 6,6 is true" w_king_7_6
      empty_6_6 true;
    is_legal_PIECES_test "king from 7,7 to 6,6 is true" w_king_7_7
      empty_6_6 true;
    is_legal_PIECES_test "king from 6,7 to 6,6 is true" w_king_6_7
      empty_6_6 true;
    is_legal_PIECES_test "king from 5,7 to 6,6 is true" w_king_5_7
      empty_6_6 true;
    is_legal_PIECES_test "king from 5,6 to 7,1 is false" w_king_5_6
      empty_7_1 false;
    is_legal_PIECES_test "knight from 4,5 to 6,6 is true" w_knight_4_5
      empty_6_6 true;
    is_legal_PIECES_test "knight from 5,4 to 6,6 is true" w_knight_5_4
      empty_6_6 true;
    is_legal_PIECES_test "knight from 7,4 to 6,6 is true" w_knight_7_4
      empty_6_6 true;
    is_legal_PIECES_test "knight from 4,7 to 6,6 is true" w_knight_4_7
      empty_6_6 true;
    is_legal_PIECES_test "knight from 3,2 to 1,1 is true" w_knight_3_2
      empty_1_1 true;
    is_legal_PIECES_test "knight from 0,3 to 1,1 is true" w_knight_0_3
      empty_1_1 true;
    is_legal_PIECES_test "knight from 3,0 to 1,1 is true" w_knight_3_0
      empty_1_1 true;
    is_legal_PIECES_test "knight from 2,3 to 1,1 is true" w_knight_2_3
      empty_1_1 true;
    is_legal_PIECES_test "knight from 2,3 to 6,6 is falses" w_knight_2_3
      empty_6_6 false;
  ]

let pieces_tests = []

(* ===============board tests below================================= *)
let get_str_piece_test name board grid expected =
  name >:: fun _ -> assert_equal expected (get_str_piece board grid)

let rec lst_to_str = function
  | [] -> ""
  | h :: t -> h ^ lst_to_str t

let get_row_test name board row_num expected =
  name >:: fun _ ->
  assert_equal expected (get_row board row_num) ~printer:lst_to_str

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

(* in_check tests - black box testing - I am testing this by using an
   online chess board to create in_check piece arrangements.*)

(* ===================================================================== *)
(* ========================TESTING [grd]============================ *)
(* ===================================================================== *)
(* This function is used to make it easier to develop tests for
   [is_check]. The board at https://www.365chess.com/board_editor.php is
   used to find alphanum locations for tests.*)

let grd (input : string) =
  let num = input.[1] |> Char.escaped |> int_of_string in
  match input.[0] with
  | 'a' -> (8 - num, 0)
  | 'b' -> (8 - num, 1)
  | 'c' -> (8 - num, 2)
  | 'd' -> (8 - num, 3)
  | 'e' -> (8 - num, 4)
  | 'f' -> (8 - num, 5)
  | 'g' -> (8 - num, 6)
  | 'h' -> (8 - num, 7)
  | _ -> failwith "not possible"

module type ColLetter = sig
  val col : string
end

module TupleOfAlphanumTester (L : ColLetter) = struct
  let col_num = function
    | "a" -> 0
    | "b" -> 1
    | "c" -> 2
    | "d" -> 3
    | "e" -> 4
    | "f" -> 5
    | "g" -> 6
    | "h" -> 7
    | _ -> failwith "Should not be reached"

  let to_grid_test name str expected =
    L.col ^ name ^ (col_num L.col |> string_of_int) ^ ")" >:: fun _ ->
    assert_equal expected (grd str)

  let tests =
    [
      (* a *)
      to_grid_test "1 -> (7," (L.col ^ "1") (7, col_num L.col);
      to_grid_test "2 -> (6," (L.col ^ "2") (6, col_num L.col);
      to_grid_test "3 -> (5," (L.col ^ "3") (5, col_num L.col);
      to_grid_test "4 -> (4," (L.col ^ "4") (4, col_num L.col);
      to_grid_test "5 -> (3," (L.col ^ "5") (3, col_num L.col);
      to_grid_test "6 -> (2," (L.col ^ "6") (2, col_num L.col);
      to_grid_test "7 -> (1," (L.col ^ "7") (1, col_num L.col);
      to_grid_test "8 -> (0," (L.col ^ "8") (0, col_num L.col);
    ]
end

module ColA : ColLetter = struct
  let col = "a"
end

module ColB : ColLetter = struct
  let col = "b"
end

module ColC : ColLetter = struct
  let col = "c"
end

module ColD : ColLetter = struct
  let col = "d"
end

module ColE : ColLetter = struct
  let col = "e"
end

module ColF : ColLetter = struct
  let col = "f"
end

module ColG : ColLetter = struct
  let col = "g"
end

module ColH : ColLetter = struct
  let col = "h"
end

module ColATester = TupleOfAlphanumTester (ColA)
module ColBTester = TupleOfAlphanumTester (ColB)
module ColCTester = TupleOfAlphanumTester (ColC)
module ColDTester = TupleOfAlphanumTester (ColD)
module ColETester = TupleOfAlphanumTester (ColE)
module ColFTester = TupleOfAlphanumTester (ColF)
module ColGTester = TupleOfAlphanumTester (ColG)
module ColHTester = TupleOfAlphanumTester (ColH)

let grd_tests =
  List.flatten
    [
      ColATester.tests;
      ColBTester.tests;
      ColCTester.tests;
      ColDTester.tests;
      ColETester.tests;
      ColFTester.tests;
      ColGTester.tests;
      ColHTester.tests;
    ]

(* ===================================================================== *)
(* ================== FINSIH TESTING [grd] ========================== *)
(* ===================================================================== *)

(* ================================================================ *)
(* ===================TESTING [is_legal_castle] =================== *)
(* ================================================================ *)
let st_1 = init_board () |> init_state

let st_2 =
  (*moves right WHT KNIGHT OUT OF WAY*)
  what_piece st_1 (grd "g1") |> update_loc st_1 (grd "g4")

let st_3 =
  (*moves WHT right BISHOP OUT OF WAY*)
  what_piece st_2 (grd "f1") |> update_loc st_2 (grd "f4")

let st_4 =
  (*moves WHT LEFT BISHOP OUT OF WAY*)
  what_piece st_3 (grd "c1") |> update_loc st_3 (grd "c4")

let st_5 =
  (*moves WHT LEFT KNIGHT OUT OF WAY*)
  what_piece st_4 (grd "b1") |> update_loc st_4 (grd "b4")

let st_6 =
  (*moves WHT WHT QUEEN OUT OF WAY*)
  what_piece st_5 (grd "d1") |> update_loc st_5 (grd "d4")

let st_7 =
  (*moves WHT WHT PAWN ABOVE KING OUT OF WAY*)
  what_piece st_6 (grd "e2") |> update_loc st_6 (grd "d3")

let st_8 =
  (*moves BLK QN so WHT KING is in check*)
  what_piece st_7 (grd "d8") |> update_loc st_7 (grd "e4")

(* below move to see if path is in check *)
let st_9 =
  (*moves WHT PAWN*)
  what_piece st_8 (grd "f2") |> update_loc st_8 (grd "e3")

let st_10 =
  (*moves BLK ROOK SO FIRST GRID OF CASTLE PATH TO RIGHT IS IN CHECK*)
  what_piece st_9 (grd "h8") |> update_loc st_9 (grd "f3")

let st_11 =
  (*moves BLK ROOK SO FIRST GRID OF CASTLE PATH TO LEFT IS IN CHECK*)
  what_piece st_10 (grd "f3") |> update_loc st_10 (grd "d2")

let st_12 =
  (*moves BLK ROOK SO 2nd GRID OF CASTLE PATH TO LEFT IS IN CHECK*)
  what_piece st_11 (grd "d2") |> update_loc st_11 (grd "c2")

let st_13 =
  (*moves BLK ROOK SO 2nd GRID OF CASTLE PATH TO RIGHT IS IN CHECK*)
  what_piece st_12 (grd "c2") |> update_loc st_12 (grd "g2")

let st_14 =
  (*moves BLK ROOK SO 2nd GRID OF CASTLE PATH TO RIGHT IS IN CHECK*)
  what_piece st_13 (grd "g2") |> update_loc st_13 (grd "h8")

let st_15 =
  (*moves WHITE ROOK TO LEFT ONCE and puts back*)
  let temp_st =
    what_piece st_14 (grd "h1")
    |> first_move
    |> update_loc st_14 (grd "h3")
  in
  what_piece temp_st (grd "h3") |> update_loc temp_st (grd "h1")

let st_16 =
  (*moves WHT WHT KING AWAY AND BACK*)
  let temp_st =
    what_piece st_15 (grd "e1")
    |> first_move
    |> update_loc st_15 (grd "e2")
  in
  what_piece temp_st (grd "e2") |> update_loc temp_st (grd "e1")

let is_legal_castle_test name st p p1 expected =
  name >:: fun _ ->
  assert_equal expected
    (is_legal_castle st p p1)
    ~printer:string_of_bool

let is_legal_castle_tests =
  (* black box *)
  [
    is_legal_castle_test
      " unmoved king CAN'T CASTLE move from (7,4) to (7,6) if pieces \
       inbtwn"
      st_1
      (what_piece st_1 (grd "e1"))
      (what_piece st_1 (grd "g1"))
      false;
    is_legal_castle_test
      " unmoved king CAN'T CASTLE move from (7,4) to (7,2) if pieces \
       in btwn"
      st_1
      (what_piece st_1 (grd "e1"))
      (what_piece st_1 (grd "c1"))
      false;
    is_legal_castle_test
      " unmoved king CAN CASTLE move from (7,4) to (7,6) if clear path "
      st_3
      (what_piece st_3 (grd "e1"))
      (what_piece st_3 (grd "g1"))
      true;
    is_legal_castle_test
      " unmoved king CAN CASTLE move from (7,4) to (7,2) if clear path "
      st_6
      (what_piece st_6 (grd "e1"))
      (what_piece st_6 (grd "c1"))
      true;
    is_legal_castle_test
      " unmoved king CAN'T CASTLE move from (7,4) to (7,6) if its in \
       check - otherwise no piece in path and "
      st_8
      (what_piece st_8 (grd "e1"))
      (what_piece st_8 (grd "g1"))
      false;
    is_legal_castle_test
      " unmoved king CAN'T CASTLE move from (7,4) to (7,6) if the \
       first square it needs to cross would be in check"
      st_10
      (what_piece st_10 (grd "e1"))
      (what_piece st_10 (grd "g1"))
      false;
    is_legal_castle_test
      " unmoved king CAN'T CASTLE move from (7,4) to (7,2) if first \
       square it needs to cross would put it in check"
      st_11
      (what_piece st_11 (grd "e1"))
      (what_piece st_11 (grd "c1"))
      false;
    is_legal_castle_test
      " unmoved king CAN'T CASTLE move from (7,4) to (7,2) if it would \
       be in check on destination"
      st_12
      (what_piece st_12 (grd "e1"))
      (what_piece st_12 (grd "c1"))
      false;
    is_legal_castle_test
      " unmoved king CAN'T CASTLE move from (7,4) to (7,6) if it would \
       be in check on destination"
      st_13
      (what_piece st_13 (grd "e1"))
      (what_piece st_13 (grd "g1"))
      false;
    is_legal_castle_test
      " unmoved king CAN'T CASTLE move from (7,4) to (7,6) if rook on \
       (7,7) has already moved"
      st_15
      (what_piece st_15 (grd "e1"))
      (what_piece st_15 (grd "g1"))
      false;
    is_legal_castle_test
      " MOVED king CAN'T CASTLE move from (7,4) to (7,2) even with \
       unmoved rook on (7,0) and clear path"
      st_16
      (what_piece st_16 (grd "e1"))
      (what_piece st_16 (grd "c1"))
      false;
  ]

(* ================================================================ *)
(* ===============FINISH TESTING [is_legal_castle] ================ *)
(* ================================================================ *)

(* ================================================================ *)
(* =============TESTING [st_with_two_pces] ======================== *)
(* ================================================================ *)
(* tests the function that creates initial state that are used to test
   [in_check] *)
let st_with_two_pces_test name pce_loc loc expected =
  name >:: fun _ ->
  assert_equal expected
    (what_piece (st_with_two_pces pce_loc) loc |> get_piece_type)

let qn_loc = (0, 3)

let rk_loc = (0, 7)

let st_with_two_pces_tests =
  [
    (* testing board with only white king and black queen *)
    st_with_two_pces_test " (0,3) is Queen" qn_loc (grd "d8") "Q";
    st_with_two_pces_test " (7,4) is King" qn_loc (grd "e1") "K";
    st_with_two_pces_test " (0,0) is no piece" qn_loc (grd "a8") "None";
    st_with_two_pces_test " (7,7) is no piece" qn_loc (grd "h1") "None";
    st_with_two_pces_test " (7,5) is no piece" qn_loc (grd "f1") "None";
    st_with_two_pces_test " (0,4) is no piece" qn_loc (grd "e8") "None";
    (* testing board with only white king and black queen *)
    st_with_two_pces_test " (0,3) is no piece" rk_loc (grd "d8") "None";
    st_with_two_pces_test " (7,4) is King" rk_loc (grd "e1") "K";
    st_with_two_pces_test " (0,7) is Rook" rk_loc (grd "h8") "R";
    st_with_two_pces_test " (0,1) is no piece" rk_loc (grd "b8") "None";
    st_with_two_pces_test " (6,3) is no piece" rk_loc (grd "d2") "None";
  ]

(* ================================================================ *)
(* =============FINSIH TESTING [st_with_two_pces] ================= *)
(* ================================================================ *)

(* ================================================================ *)
(* ===================TESTING [diag_check_piece] ================== *)
(* ================================================================ *)
(* testing to make sure [is_check] works properly. [is_check] calls many
   helpers, so bugs in it could be in any of them. *)
let diag_check_piece_test name p p1 p2 kg_color expected =
  name >:: fun _ ->
  assert_equal expected
    (diag_check_piece p p1 p2 kg_color)
    ~printer:get_piece_type

let diag_check_piece_tests =
  (* bishop/queen *)
  (* black box *)
  [
    diag_check_piece_test "p = p2 (w_kng), p3 is blk queen" w_king_5_6
      w_king_5_6 b_queen_0_3 (get_color w_king_5_6) b_queen_0_3;
    diag_check_piece_test "p = p2 (w_kng), p3 is blk bishop" w_king_5_6
      w_king_5_6 b_bishop_4_4 (get_color w_king_5_6) b_bishop_4_4;
    diag_check_piece_test "p = p2 (w_kng), p3 is blk rook" w_king_5_6
      w_king_5_6 b_rook_0_0 (get_color w_king_5_6) w_king_5_6;
    diag_check_piece_test "p = p2 (w_kng), p3 is blk knight" w_king_5_6
      w_king_5_6 b_knight_4_2 (get_color w_king_5_6) w_king_5_6;
    diag_check_piece_test "p = p2 (w_kng), p3 is blk pawn" w_king_5_6
      w_king_5_6 b_pawn (get_color w_king_5_6) w_king_5_6;
    diag_check_piece_test "p = p2 (w_kng), p3 is wht queen" w_king_5_6
      w_king_5_6 w_queen_7_3 (get_color w_king_5_6) w_king_5_6;
    diag_check_piece_test "p = p2 (w_kng), p3 is wht bishop" w_king_5_6
      w_king_5_6 w_bishop_4_4 (get_color w_king_5_6) w_king_5_6;
    (* below for same color non queen/bishop *)
    diag_check_piece_test "p = p2 (w_kng), p3 is wht rook" w_king_5_6
      w_king_5_6 w_rook_1_4 (get_color w_king_5_6) w_king_5_6;
    diag_check_piece_test
      "p is w_kng; p2 is blk queen, p3 is blk bishop" w_king_5_6
      b_queen_0_3 b_bishop_4_4 (get_color w_king_5_6) b_queen_0_3;
    diag_check_piece_test
      "p is w_kng; p2 is blk bishop, p3 is blk queen" w_king_5_6
      b_bishop_4_4 b_queen_0_3 (get_color w_king_5_6) b_bishop_4_4;
  ]

(* ================================================================ *)
(* =============FINSIH TESTING [diag_check_piece] ================= *)
(* ================================================================ *)

(* ================================================================ *)
(* ================TESTING [orthog_adj_check_piece] =============== *)
(* ================================================================ *)
(* testing to make sure [is_check] works properly. [is_check] calls many
   helpers, so bugs in it could be in any of them. *)
let orthog_adj_check_piece_test name p p1 p2 kg_color expected =
  name >:: fun _ ->
  assert_equal expected
    (orthog_adj_check_piece p p1 p2 kg_color)
    ~printer:get_piece_type

let orthog_adj_check_piece_tests =
  (* rook/queen accepted*)
  (* black box *)
  [
    orthog_adj_check_piece_test
      " orthog_check: p = p2 (w_kng), p3 is blk queen" w_king_5_6
      w_king_5_6 b_queen_0_3 (get_color w_king_5_6) b_queen_0_3;
    orthog_adj_check_piece_test
      " orthog_check: p = p2 (w_kng), p3 is blk bishop" w_king_5_6
      w_king_5_6 b_bishop_4_4 (get_color w_king_5_6) w_king_5_6;
    orthog_adj_check_piece_test
      " orthog_check: p = p2 (w_kng), p3 is blk rook" w_king_5_6
      w_king_5_6 b_rook_0_0 (get_color w_king_5_6) b_rook_0_0;
    orthog_adj_check_piece_test
      " orthog_check: p = p2 (w_kng), p3 is blk knight" w_king_5_6
      w_king_5_6 b_knight_4_2 (get_color w_king_5_6) w_king_5_6;
    orthog_adj_check_piece_test
      " orthog_check: p = p2 (w_kng), p3 is blk pawn" w_king_5_6
      w_king_5_6 b_pawn (get_color w_king_5_6) w_king_5_6;
    orthog_adj_check_piece_test
      " orthog_check: p = p2 (w_kng), p3 is wht queen" w_king_5_6
      w_king_5_6 w_queen_7_3 (get_color w_king_5_6) w_king_5_6;
    orthog_adj_check_piece_test
      " orthog_check: p = p2 (w_kng), p3 is wht bishop" w_king_5_6
      w_king_5_6 w_bishop_4_4 (get_color w_king_5_6) w_king_5_6;
    orthog_adj_check_piece_test
      " orthog_check: p = p2 (w_kng), p3 is wht rook" w_king_5_6
      w_king_5_6 w_rook_1_4 (get_color w_king_5_6) w_king_5_6;
    orthog_adj_check_piece_test
      " orthog_check: p is w_kng; p2 is blk queen, p3 is blk bishop"
      w_king_5_6 b_queen_0_3 b_bishop_4_4 (get_color w_king_5_6)
      b_queen_0_3;
    (* ATTENTION!!!!!!!!!!!!!!!!!!!! *)
    (* CHECK WITH MAX ON THESE TESTS BELOW *)
    orthog_adj_check_piece_test
      " orthog_check: p is w_kng; p2 is blk bishop, p3 is blk queen"
      w_king_5_6 b_bishop_4_4 b_queen_0_3 (get_color w_king_5_6)
      w_king_5_6;
    (* ATTENTION!!!!!!!!!!!!!!!!!!!! *)
    (* CHECK WITH MAX ON THESE TESTS BELOW *)
    orthog_adj_check_piece_test
      " orthog_check: p is w_kng; p2 is blk knight, p3 is blk rook"
      w_king_5_6 b_knight_4_2 b_rook_0_0 (get_color w_king_5_6)
      w_king_5_6;
  ]

(* ================================================================ *)
(* ===========FINISH TESTING [orthog_adj_check_piece] ============= *)
(* ================================================================ *)

(* ================================================================ *)
(* ===================TESTING [knight_check_piece] ================== *)
(* ================================================================ *)
(* testing to make sure [is_check] works properly. [is_check] calls many
   helpers, so bugs in it could be in any of them. *)
let knight_check_piece_test name p p2 kg_color expected =
  name >:: fun _ ->
  assert_equal expected
    (knight_check_piece p p2 kg_color)
    ~printer:get_piece_type

let knight_check_piece_tests =
  (* black box *)
  [
    knight_check_piece_test
      " KNIGHT CHECK TEST: p is w_kng, p2 is blk queen" w_king_5_6
      b_queen_0_3 (get_color w_king_5_6) w_king_5_6;
    knight_check_piece_test
      " KNIGHT CHECK TEST: p is w_kng, p2 is blk bishop" w_king_5_6
      b_bishop_4_4 (get_color w_king_5_6) w_king_5_6;
    knight_check_piece_test
      " KNIGHT CHECK TEST: p w_kng, p2 is blk rook" w_king_5_6
      b_rook_0_0 (get_color w_king_5_6) w_king_5_6;
    knight_check_piece_test
      " KNIGHT CHECK TEST: p is w_kng, p2 is blk knight" w_king_5_6
      b_knight_4_2 (get_color w_king_5_6) b_knight_4_2;
    knight_check_piece_test
      " KNIGHT CHECK TEST: p is w_kng, p2 is wht knight" w_king_5_6
      w_knight_4_5 (get_color w_king_5_6) w_king_5_6;
    knight_check_piece_test
      " KNIGHT CHECK TEST: p w_kng, p2 is blk pawn" w_king_5_6 b_pawn
      (get_color w_king_5_6) w_king_5_6;
    knight_check_piece_test
      " KNIGHT CHECK TEST: p is w_kng, p2 is wht queen" w_king_5_6
      w_queen_7_3 (get_color w_king_5_6) w_king_5_6;
    knight_check_piece_test
      " KNIGHT CHECK TEST: p is w_kng, p2 is wht bishop" w_king_5_6
      w_bishop_4_4 (get_color w_king_5_6) w_king_5_6;
    knight_check_piece_test
      " KNIGHT CHECK TEST: p w_kng, p2 is wht rook" w_king_5_6
      w_rook_1_4 (get_color w_king_5_6) w_king_5_6;
    (* when [p] is not a king *)
    knight_check_piece_test
      " KNIGHT CHECK TEST: p is w_queen, p2 is blk knight" w_queen_7_3
      b_knight_4_2 (get_color w_queen_7_3) b_knight_4_2;
  ]

(* ================================================================ *)
(* =============FINSIH TESTING [knight_check_piece] =============== *)
(* ================================================================ *)

(* ================================================================ *)
(* ===================TESTING [piece_in_pth] ====================== *)
(* ================================================================ *)
(* testing to make sure [is_check] works properly. [is_check] calls many
   helpers, so bugs in it could be in any of them. *)
let st_1 = init_board () |> init_state

let st_2 =
  (*moves WHT KING*)
  what_piece st_1 (grd "e1") |> update_loc st_1 (grd "e4")

let st_3 =
  (*moves BLK BISHOP*)
  what_piece st_2 (grd "f8") |> update_loc st_2 (grd "e5")

let st_4 =
  (*moves WHT ROOK*)
  what_piece st_3 (grd "a1") |> update_loc st_3 (grd "b4")

let st_5 =
  (*moves WHT KNIGHT*)
  what_piece st_4 (grd "g1") |> update_loc st_4 (grd "f3")

let st_6 =
  (*moves BLK KNIGHT*)
  what_piece st_5 (grd "g8") |> update_loc st_5 (grd "g6")

let st_7 =
  (*moves WHT PAWN*)
  what_piece st_6 (grd "h2") |> update_loc st_6 (grd "h4")

let piece_in_path_test name st l1 l2 expected =
  name >:: fun _ ->
  assert_equal expected (piece_in_path st l1 l2) ~printer:get_piece_type

let piece_in_path_tests =
  (* black box *)
  [
    (* path vertical to above *)
    piece_in_path_test
      "PIECE_IN_PATH TEST: Piece btwn R at (7,7)/h1 and P at (1,7)/h7 \
       is P at (6,7)/h2"
      st_1 (grd "h1") (grd "h7")
      (what_piece st_1 (grd "h2"));
    (* path horizontal to left *)
    piece_in_path_test
      "PIECE_IN_PATH TEST: Piece btwn R at (7,7)/h1 and K at (7,4)/e1 \
       is N at (7,6)/g1"
      st_1 (grd "h1") (grd "e1")
      (what_piece st_1 (grd "g1"));
    (* path diagonal to upper left *)
    piece_in_path_test
      "PIECE_IN_PATH TEST: Piece btwn R at (7,7)/h1 and None at \
       (5,5)/f3 is P at (6,6)/g2"
      st_1 (grd "h1") (grd "f3")
      (what_piece st_1 (grd "g2"));
    (* Below tests use state [st_4] created above.*)
    (* adjacent above *)
    piece_in_path_test
      "PIECE_IN_PATH TEST: Piece btwn K at (4,4)/e4 and B at (3,4)/e5 \
       is B at (3,4)/e5"
      st_4 (grd "e4") (grd "e5")
      (what_piece st_4 (grd "e5"));
    (* path vertical above *)
    piece_in_path_test
      "PIECE_IN_PATH TEST: Piece btwn K at (4,4)/e4 and K at (0,4)/e8 \
       is B at (3,4)/e5"
      st_4 (grd "e4") (grd "e8")
      (what_piece st_4 (grd "e5"));
    (* horizontal left *)
    piece_in_path_test
      "PIECE_IN_PATH TEST: Piece btwn K at (4,4)/e4 and R at (4,1)/b4 \
       is R at (4,1)/b4"
      st_4 (grd "e4") (grd "b4")
      (what_piece st_4 (grd "b4"));
    (* diagonal path to lower left *)
    piece_in_path_test
      "PIECE_IN_PATH TEST: Piece btwn K at (4,4)/e4 and N at (7,1)/b1 \
       is P at (6,2)/c2"
      st_4 (grd "e4") (grd "b1")
      (what_piece st_4 (grd "c2"));
    (* Below tests use state [st_7] created above. *)
    (* diagonal path to lower right *)
    piece_in_path_test
      "PIECE_IN_PATH TEST: Piece btwn K at (4,4)/e4 and R at (7,7)/h1 \
       is N at (5,5)/f3"
      st_7 (grd "e4") (grd "h1")
      (what_piece st_7 (grd "f3"));
    (* horizontal path to right *)
    piece_in_path_test
      "PIECE_IN_PATH TEST: Piece btwn K at (4,4)/e4 and P at (4,7)/h4 \
       is P at (4,7)/h4"
      st_7 (grd "e4") (grd "h4")
      (what_piece st_7 (grd "h4"));
    (* diagonal path to upper right *)
    piece_in_path_test
      "PIECE_IN_PATH TEST: Piece btwn K at (4,4)/e4 and P at (1,7)/h7 \
       is N at (2,6)/g6"
      st_7 (grd "e4") (grd "h7")
      (what_piece st_7 (grd "g6"));
  ]

(* ================================================================ *)
(* ================FINSIH TESTING [piece_in_path] ================= *)
(* ================================================================ *)

(* ================================================================ *)
(* =============TESTING [in_check] W/ BLK PIECE, WHT K============= *)
(* ================================================================ *)

module type PieceType = sig
  val start_loc : int * int

  val pce_str : string
end

module InCheckTester (Pce : PieceType) = struct
  (* The initial state [qn_check_test_state] only contains the black
     queen and white king so as to check the effect opposing queen on
     the king with no possible interference.*)

  (* Below are the staets to use in testing in_check. We create an
     initial state then udpate it for desired arrangement. *)
  let init_check_test_state = st_with_two_pces Pce.start_loc

  let st1 =
    (*moves queen*)
    what_piece init_check_test_state Pce.start_loc
    |> update_loc init_check_test_state (grd "d6")

  let check_st2 =
    (*moves KING*)
    what_piece st1 (grd "e1") |> update_loc st1 (grd "d4")

  let check_st3 =
    (*moves queen*)
    what_piece check_st2 (grd "d6") |> update_loc check_st2 (grd "e6")

  let check_st4 =
    (*moves queen*)
    what_piece check_st3 (grd "e6") |> update_loc check_st3 (grd "f6")

  let check_st5 =
    (*moves queen*)
    what_piece check_st4 (grd "f6") |> update_loc check_st4 (grd "f5")

  let check_st6 =
    (*moves queen*)
    what_piece check_st5 (grd "f5") |> update_loc check_st5 (grd "f4")

  let check_st7 =
    (*moves queen*)
    what_piece check_st6 (grd "f4") |> update_loc check_st6 (grd "f3")

  let check_st8 =
    (*moves queen*)
    what_piece check_st7 (grd "f3") |> update_loc check_st7 (grd "f2")

  let check_st9 =
    (*moves queen*)
    what_piece check_st8 (grd "f2") |> update_loc check_st8 (grd "e2")

  let check_st10 =
    (*moves queen*)
    what_piece check_st9 (grd "e2") |> update_loc check_st9 (grd "d2")

  let check_st11 =
    (*moves queen*)
    what_piece check_st10 (grd "d2") |> update_loc check_st10 (grd "c2")

  let check_st12 =
    (*moves queen*)
    what_piece check_st11 (grd "c2") |> update_loc check_st11 (grd "b2")

  let check_st13 =
    (*moves queen*)
    what_piece check_st12 (grd "b2") |> update_loc check_st12 (grd "b3")

  let check_st14 =
    (*moves queen*)
    what_piece check_st13 (grd "b3") |> update_loc check_st13 (grd "b4")

  let check_st15 =
    (*moves queen*)
    what_piece check_st14 (grd "b4") |> update_loc check_st14 (grd "b5")

  let check_st16 =
    (*moves queen*)
    what_piece check_st15 (grd "b5") |> update_loc check_st15 (grd "b6")

  let check_st17 =
    (*moves queen*)
    what_piece check_st16 (grd "b6") |> update_loc check_st16 (grd "c6")

  let check_st18 =
    (*moves queen*)
    what_piece check_st17 (grd "c6") |> update_loc check_st17 (grd "h8")

  let check_st19 =
    (*moves queen*)
    what_piece check_st18 (grd "h8") |> update_loc check_st18 (grd "d3")

  let st20 =
    (*moves KING*)
    what_piece check_st19 (grd "d4") |> update_loc check_st19 (grd "a8")

  let check_st21 =
    (*moves queen*)
    what_piece st20 (grd "d3") |> update_loc st20 (grd "d8")

  (* The king pieces used in tests *)
  let wking_loc43 = what_piece check_st2 (grd "d4")

  let wking_loc00 = what_piece st20 (grd "a8")

  (* The different orientations a pce can have around king. Used for
     testing in_check for each piece. *)
  type pos_orient =
    | Vert
    | Hori
    | Dia
    | OnlyKngt

  let pos_orient_str = function
    | Vert -> "VERTICAL"
    | Hori -> "HORIZONTAL"
    | Dia -> "DIAGONAL"
    | OnlyKngt -> "ONLY KNIGHT (L)"

  (* vert, hori, dia, only_knight ways in which check might happen*)
  let detrmne_chk pce_pos pce_type =
    match pce_type with
    | "QUEEN" -> (
        match pce_pos with
        | Vert -> true
        | Hori -> true
        | Dia -> true
        | OnlyKngt -> false)
    | "ROOK" -> (
        match pce_pos with
        | Vert -> true
        | Hori -> true
        | Dia -> false
        | OnlyKngt -> false)
    | "BISHOP" -> (
        match pce_pos with
        | Vert -> false
        | Hori -> false
        | Dia -> true
        | OnlyKngt -> false)
    | "KNIGHT" -> (
        match pce_pos with
        | Vert -> false
        | Hori -> false
        | Dia -> false
        | OnlyKngt -> true)
    | _ -> failwith "Should never reach this."

  let in_check_test name state piece p_color pce_arrange =
    let expected = detrmne_chk pce_arrange Pce.pce_str in
    "     TESTING [in_check]:" ^ name ^ ". [in_check] SHOULD BE "
    ^ (expected |> string_of_bool)
    ^ ". CHECK DIRECTION IS: "
    ^ pos_orient_str pce_arrange
    >:: fun _ ->
    assert_equal expected
      (in_check state piece p_color)
      ~printer:string_of_bool

  (* THE BELOW TEST WITH ONLY KING AND OTHER PIECE, NO PIECES IN BETWEEN
     THEM *)
  let in_check_tests =
    [
      (* THE BLK PCE CIRCLES THE KING IN A CIRCLE ONE GRID AWAY *)
      in_check_test
        (" BLK " ^ Pce.pce_str ^ " at 2,3/d6; WHT K at 4,3")
        check_st2 wking_loc43 (get_color wking_loc43) Vert;
      in_check_test
        (" BLK " ^ Pce.pce_str ^ " at 2,4/e6; WHT K at 4,3")
        check_st3 wking_loc43 (get_color wking_loc43) OnlyKngt;
      in_check_test
        (" BLK " ^ Pce.pce_str ^ " at 2,5/f6; WHT K at 4,3")
        check_st4 wking_loc43 (get_color wking_loc43) Dia;
      in_check_test
        (" BLK " ^ Pce.pce_str ^ " at 3,5/f5; WHT K at 4,3")
        check_st5 wking_loc43 (get_color wking_loc43) OnlyKngt;
      in_check_test
        (" BLK " ^ Pce.pce_str ^ " at 4,5/f4; WHT K at 4,3")
        check_st6 wking_loc43 (get_color wking_loc43) Hori;
      in_check_test
        (" BLK " ^ Pce.pce_str ^ " at 5,5/f3; WHT K at 4,3")
        check_st7 wking_loc43 (get_color wking_loc43) OnlyKngt;
      in_check_test
        (" BLK " ^ Pce.pce_str ^ " at 6,5/f2; WHT K at 4,3")
        check_st8 wking_loc43 (get_color wking_loc43) Dia;
      in_check_test
        (" BLK " ^ Pce.pce_str ^ " at 6,4/e2; WHT K at 4,3")
        check_st9 wking_loc43 (get_color wking_loc43) OnlyKngt;
      in_check_test
        (" BLK " ^ Pce.pce_str ^ " at 6,3/d2; WHT K at 4,3")
        check_st10 wking_loc43 (get_color wking_loc43) Vert;
      in_check_test
        (" BLK " ^ Pce.pce_str ^ " at 6,2/c2 WHT K at 4,3")
        check_st11 wking_loc43 (get_color wking_loc43) OnlyKngt;
      in_check_test
        (" BLK " ^ Pce.pce_str ^ " at 6,1/b2; WHT K at 4,3")
        check_st12 wking_loc43 (get_color wking_loc43) Dia;
      in_check_test
        (" BLK " ^ Pce.pce_str ^ " at 5,1/b3 WHT K at 4,3")
        check_st13 wking_loc43 (get_color wking_loc43) OnlyKngt;
      in_check_test
        (" BLK " ^ Pce.pce_str ^ " at 4,1/b4; WHT K at 4,3")
        check_st14 wking_loc43 (get_color wking_loc43) Hori;
      in_check_test
        (" BLK " ^ Pce.pce_str ^ " at 3,1/b5 WHT K at 4,3")
        check_st15 wking_loc43 (get_color wking_loc43) OnlyKngt;
      in_check_test
        (" BLK " ^ Pce.pce_str ^ " at 2,1/b6; WHT K at 4,3")
        check_st16 wking_loc43 (get_color wking_loc43) Dia;
      in_check_test
        (" BLK " ^ Pce.pce_str ^ " at 2,2/c6 WHT K at 4,3")
        check_st17 wking_loc43 (get_color wking_loc43) OnlyKngt;
      (* CASES THAT TRY BLK PIECE FURTHER OR CLOSER THAN 1 GRID BTWN *)
      in_check_test
        (" (MORE THAN 2 GRIDS BTWN K & Q)- BLK " ^ Pce.pce_str
       ^ " at 0,7/h8; WHT K at 4,3")
        check_st18 wking_loc43 (get_color wking_loc43) Dia;
      in_check_test
        (" (DIRECTLY UNDER KG) BLK " ^ Pce.pce_str
       ^ " at 5,3/d3; WHT K at 4,3")
        check_st19 wking_loc43 (get_color wking_loc43) Vert;
      (* KING IN A CORNER - 0,0/a8 *)
      in_check_test
        (" (KG IN CORNER) BLK " ^ Pce.pce_str
       ^ " at 0,3/d8; WHT K at 0,0")
        check_st21 wking_loc00 (get_color wking_loc00) Hori;
    ]
end

module Queen : PieceType = struct
  let start_loc = grd "d8"

  let pce_str = "QUEEN"
end

module Rook : PieceType = struct
  let start_loc = grd "h8"

  let pce_str = "ROOK"
end

module Bishop : PieceType = struct
  let start_loc = grd "f8"

  let pce_str = "BISHOP"
end

module Knight : PieceType = struct
  let start_loc = grd "g8"

  let pce_str = "KNIGHT"
end

module QueenInCheckTests = InCheckTester (Queen)
module RookInCheckTests = InCheckTester (Rook)
module BishopInCheckTests = InCheckTester (Bishop)
module KnightInCheckTests = InCheckTester (Knight)

(* ================================================================ *)
(* ========== FINISH TESTING [in_check] =========================== *)
(* ================================================================ *)

(* ================================================================ *)
(* ======================TESTING [checkmate] ====================== *)
(* ================================================================ *)
(* let checkmate_test name state kg_color expected = name >:: fun _ ->
   assert_equal expected (checkmate state kg_color) *)

let checkmate_tests =
  (* black box *)
  [
    (* path vertical to above *)
    piece_in_path_test
      "PIECE_IN_PATH TEST: Piece btwn R at (7,7)/h1 and P at (1,7)/h7 \
       is P at (6,7)/h2"
      st_1 (grd "h1") (grd "h7")
      (what_piece st_1 (grd "h2"));
  ]

(* ================================================================ *)
(* ==================FINSIH TESTING [checkmate] =================== *)
(* ================================================================ *)

let tests =
  "test suite for Chess"
  >::: List.flatten
         [
           is_legal_PIECES_tests;
           pieces_tests;
           board_tests;
           state_tests;
           QueenInCheckTests.in_check_tests;
           RookInCheckTests.in_check_tests;
           BishopInCheckTests.in_check_tests;
           KnightInCheckTests.in_check_tests;
           st_with_two_pces_tests;
           grd_tests;
           diag_check_piece_tests;
           orthog_adj_check_piece_tests;
           knight_check_piece_tests;
           piece_in_path_tests;
           is_legal_castle_tests;
         ]

let _ = run_test_tt_main tests