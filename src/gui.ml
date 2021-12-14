open Pieces
open Graphics
open State

type rectangle = string

let set_background color =
  draw_rect 0 0 800 800;
  set_color color;
  fill_rect 0 0 800 800

let board_background x y s color =
  draw_rect x y s s;
  set_color color;
  fill_rect x y s s

let color_rec x y s b =
  draw_rect x y s s;
  if b then set_color (rgb 91 92 91) else set_color (rgb 142 145 142);
  fill_rect x y s s

let rec draw_matrix_rows x y s row bl =
  match row with
  | 1 -> color_rec x y s bl
  | _ ->
      color_rec x y s bl;
      draw_matrix_rows (x + s) y s (row - 1)
        (if bl then false else true)

let rec draw_matrix x y s row col bl =
  match col with
  | 1 -> draw_matrix_rows x y s row bl
  | _ ->
      draw_matrix_rows x y s row bl;
      draw_matrix x (y + s) s row (col - 1) (if bl then false else true)

let words loc_x loc_y phrase s =
  moveto loc_x loc_y;
  set_color black;
  draw_string phrase

let rec notation_num s loc_x loc_y lst =
  match lst with
  | [] -> draw_string ""
  | h :: t ->
      moveto loc_x loc_y;
      set_color (rgb 175 179 175);
      draw_string h;
      notation_num s loc_x (loc_y + s) t

let rec notation_words s loc_x loc_y lst =
  match lst with
  | [] -> draw_string ""
  | h :: t ->
      moveto loc_x loc_y;
      set_color (rgb 175 179 175);
      draw_string h;
      notation_words s (loc_x + s) loc_y t

let notation s loc_x loc_y =
  notation_num s loc_x
    (loc_y + (s / 2) + 20)
    [ "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8" ];
  notation_words s
    (loc_x + (s / 2) + 20)
    (loc_y + (s * 8) + (s / 2) - 5)
    [ "A"; "B"; "C"; "D"; "E"; "F"; "G"; "H" ]

let rec pawns count s loc_x loc_y bl =
  match count with
  | 0 ->
      moveto loc_x loc_y;
      if bl then set_color black else set_color white;
      draw_string "PAWN"
  | _ ->
      moveto loc_x loc_y;
      if bl then set_color black else set_color white;
      draw_string "PAWN";
      pawns (count - 1) s (loc_x + s) loc_y bl

let rec pieces s loc_x loc_y bl lst =
  match lst with
  | [] ->
      moveto loc_x loc_y;
      if bl then set_color black else set_color white;
      draw_string ""
  | h :: t ->
      moveto loc_x loc_y;
      if bl then set_color black else set_color white;
      draw_string h;
      pieces s (loc_x + s) loc_y bl t

let init_setup init_x init_y s =
  pawns 7 s (init_x + (s / 3)) (init_y + (s + (s / 2)) - 10) false;
  pieces s
    (init_x + (s / 3))
    (init_y + (s / 2) - 10)
    false
    [
      "ROOK";
      "KNIGHT";
      "BISHOP";
      "QUEEN";
      "KING";
      "BISHOP";
      "KNIGHT";
      "ROOK";
    ];
  pawns 7 s (init_x + (s / 3)) ((init_y * 8) - 10) true;
  pieces s
    (init_x + (s / 3))
    ((init_y * 9) - 10)
    true
    [
      "ROOK";
      "KNIGHT";
      "BISHOP";
      "KING";
      "QUEEN";
      "BISHOP";
      "KNIGHT";
      "ROOK";
    ]

let new_board () =
  open_graph " 800x800";
  set_window_title "Chess Game";
  set_background (rgb 23 48 27);
  board_background 40 40 710 (rgb 99 46 21);
  board_background 70 70 650 (rgb 142 145 142);
  draw_matrix 75 75 80 8 8 true;
  notation 80 53 55

let draw_string_loc x y bl str =
  if bl then set_color black else set_color white;
  moveto x y;
  draw_string str

let init =
  new_board ();
  init_setup 75 75 80

let matrix_notation_to_string_loc_x notation =
  let x_pos = snd notation in
  (x_pos * 80) + 101

let matrix_notation_to_string_loc_y notation =
  let y_pos = fst notation in
  665 - (y_pos * 80)

let position_piece notation piece =
  let color = if get_color piece = "B" then true else false in
  let draw_string_piece =
    draw_string_loc
      (matrix_notation_to_string_loc_x notation)
      (matrix_notation_to_string_loc_y notation)
      color
  in
  match get_piece_type piece with
  | "P" -> draw_string_piece "PAWN"
  | "B" -> draw_string_piece "BISHOP"
  | "N" -> draw_string_piece "KNIGHT"
  | "R" -> draw_string_piece "ROOK"
  | "Q" -> draw_string_piece "QUEEN"
  | "K" -> draw_string_piece "KING"
  | _ -> ()

(*let position_piece_test notation piece = let draw_string_piece =
  draw_string_loc (matrix_notation_to_string_loc_x notation)
  (matrix_notation_to_string_loc_y notation) in match piece with | "P"
  -> draw_string_piece "PAWN" | "B" -> draw_string_piece "BISHOP" | "N"
  -> draw_string_piece "KNIGHT" | "R" -> draw_string_piece "ROOK" | "Q"
  -> draw_string_piece "QUEEN" | "K" -> draw_string_piece "KING" | _ ->
  ()*)

(*let rec new_board_with_pieces a_list = new_board (); match a_list with
  | [] -> () | h :: t -> position_piece_test (fst h) (snd h);
  new_board_with_pieces t*)
let new_board_with_pieces a_list x_pos y_pos =
  new_board ();
  let rec draw_through_y single_x_pos single_y_pos =
    match single_y_pos with
    | -1 -> ()
    | y ->
        position_piece (single_x_pos, y)
          (what_piece a_list (single_x_pos, y));
        draw_through_y single_x_pos (y - 1)
  in
  let rec draw_through_x curr_x =
    match curr_x with
    | -1 -> ()
    | x ->
        draw_through_y x y_pos;
        draw_through_x (x - 1)
  in
  draw_through_x x_pos

let conv_to_x coor = (coor - 75) / 80

let conv_to_y coor = 7 - conv_to_x coor

let conv_to_loc st =
  new_board_with_pieces st 7 7;
  let det_position =
    match wait_next_event [ Button_down ] with
    | { mouse_x; mouse_y } -> (mouse_x, mouse_y)
  in
  (conv_to_y (snd det_position), conv_to_x (fst det_position))

let promotion_gui () =
  board_background 75 75 640 (rgb 0 0 0);
  board_background 315 315 160 (rgb 255 255 255);
  moveto 310 520;
  draw_string "Select a piece to promote to!";
  set_color black;
  draw_string_loc 335 355 true "Queen";
  draw_string_loc 415 355 true "Rook";
  draw_string_loc 335 435 true "Bishop";
  draw_string_loc 415 435 true "Knight";
  let det_position =
    match wait_next_event [ Button_down ] with
    | { mouse_x; mouse_y } -> (mouse_x, mouse_y)
  in
  let fst_det_pos = fst det_position in
  let snd_det_pos = snd det_position in
  if
    fst_det_pos < 315 || fst_det_pos > 475 || snd_det_pos < 315
    || snd_det_pos > 475
  then "reselect"
  else if fst_det_pos < 395 then if snd_det_pos < 395 then "Q" else "B"
  else if snd_det_pos < 395 then "R"
  else "N"
