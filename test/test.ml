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
   [is_check]. *)

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
    st_with_two_pces_test " (0,0) is no piece" qn_loc (grd "a8") "";
    st_with_two_pces_test " (7,7) is no piece" qn_loc (grd "h1") "";
    st_with_two_pces_test " (7,5) is no piece" qn_loc (grd "f1") "";
    st_with_two_pces_test " (0,4) is no piece" qn_loc (grd "e8") "";
    (* testing board with only white king and black queen *)
    st_with_two_pces_test " (0,3) is no piece" rk_loc (grd "d8") "";
    st_with_two_pces_test " (7,4) is King" rk_loc (grd "e1") "K";
    st_with_two_pces_test " (0,7) is Rook" rk_loc (grd "h8") "R";
    st_with_two_pces_test " (0,1) is no piece" rk_loc (grd "b8") "";
    st_with_two_pces_test " (6,3) is no piece" rk_loc (grd "d2") "";
  ]

(* ================================================================ *)
(* =============FINSIH TESTING [st_with_two_pces] ================= *)
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
  let wking_loc43 = what_piece st1 (grd "d4")

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

  let in_check_test name state piece pce_arrange =
    let expected = detrmne_chk pce_arrange Pce.pce_str in
    "     TESTING [in_check]:" ^ name ^ ". [in_check] SHOULD BE "
    ^ (expected |> string_of_bool)
    ^ ". CHECK DIRECTION IS: "
    ^ pos_orient_str pce_arrange
    >:: fun _ ->
    assert_equal expected (in_check state piece) ~printer:string_of_bool

  (* THE BELOW TEST WITH ONLY KING AND OTHER PIECE, NO PIECES IN BETWEEN
     THEM *)
  let in_check_tests =
    [
      (* THE BLK PCE CIRCLES THE KING IN A CIRCLE ONE GRID AWAY *)
      in_check_test
        (" BLK " ^ Pce.pce_str ^ " at 2,3/d6; WHT K at 4,3")
        check_st2 wking_loc43 Vert;
      in_check_test
        (" BLK " ^ Pce.pce_str ^ " at 2,4/e6; WHT K at 4,3")
        check_st3 wking_loc43 OnlyKngt;
      in_check_test
        (" BLK " ^ Pce.pce_str ^ " at 2,5/f6; WHT K at 4,3")
        check_st4 wking_loc43 Dia;
      in_check_test
        (" BLK " ^ Pce.pce_str ^ " at 3,5/f5; WHT K at 4,3")
        check_st5 wking_loc43 OnlyKngt;
      in_check_test
        (" BLK " ^ Pce.pce_str ^ " at 4,5/f4; WHT K at 4,3")
        check_st6 wking_loc43 Hori;
      in_check_test
        (" BLK " ^ Pce.pce_str ^ " at 5,5/f3; WHT K at 4,3")
        check_st7 wking_loc43 OnlyKngt;
      in_check_test
        (" BLK " ^ Pce.pce_str ^ " at 6,5/f2; WHT K at 4,3")
        check_st8 wking_loc43 Dia;
      in_check_test
        (" BLK " ^ Pce.pce_str ^ " at 6,4/e2; WHT K at 4,3")
        check_st9 wking_loc43 OnlyKngt;
      in_check_test
        (" BLK " ^ Pce.pce_str ^ " at 6,3/d2; WHT K at 4,3")
        check_st10 wking_loc43 Vert;
      in_check_test
        (" BLK " ^ Pce.pce_str ^ " at 6,2/c2 WHT K at 4,3")
        check_st11 wking_loc43 OnlyKngt;
      in_check_test
        (" BLK " ^ Pce.pce_str ^ " at 6,1/b2; WHT K at 4,3")
        check_st12 wking_loc43 Dia;
      in_check_test
        (" BLK " ^ Pce.pce_str ^ " at 5,1/b3 WHT K at 4,3")
        check_st13 wking_loc43 OnlyKngt;
      in_check_test
        (" BLK " ^ Pce.pce_str ^ " at 4,1/b4; WHT K at 4,3")
        check_st14 wking_loc43 Hori;
      in_check_test
        (" BLK " ^ Pce.pce_str ^ " at 3,1/b5 WHT K at 4,3")
        check_st15 wking_loc43 OnlyKngt;
      in_check_test
        (" BLK " ^ Pce.pce_str ^ " at 2,1/b6; WHT K at 4,3")
        check_st16 wking_loc43 Dia;
      in_check_test
        (" BLK " ^ Pce.pce_str ^ " at 2,2/c6 WHT K at 4,3")
        check_st17 wking_loc43 OnlyKngt;
      (* CASES THAT TRY BLK PIECE FURTHER OR CLOSER THAN 1 GRID BTWN *)
      in_check_test
        (" (MORE THAN 2 GRIDS BTWN K & Q)- BLK " ^ Pce.pce_str
       ^ " at 0,7/h8; WHT K at 4,3")
        check_st18 wking_loc43 Dia;
      in_check_test
        (" (DIRECTLY UNDER KG) BLK " ^ Pce.pce_str
       ^ " at 5,3/d3; WHT K at 4,3")
        check_st19 wking_loc43 Vert;
      (* KING IN A CORNER - 0,0/a8 *)
      in_check_test
        (" (KG IN CORNER) BLK " ^ Pce.pce_str
       ^ " at 0,3/d8; WHT K at 0,0")
        check_st21 wking_loc00 Hori;
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

let tests =
  "test suite for Chess"
  >::: List.flatten
         [
           is_legal_tests;
           pieces_tests;
           board_tests;
           state_tests;
           QueenInCheckTests.in_check_tests;
           RookInCheckTests.in_check_tests;
           BishopInCheckTests.in_check_tests;
           KnightInCheckTests.in_check_tests;
           st_with_two_pces_tests;
           grd_tests;
         ]

let _ = run_test_tt_main tests