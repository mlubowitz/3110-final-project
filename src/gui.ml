open Graphics

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

let init =
  open_graph " 800x800";
  set_window_title "Chess Game";
  set_background (rgb 23 48 27);
  board_background 40 40 710 (rgb 99 46 21);
  board_background 70 70 650 (rgb 142 145 142);
  draw_matrix 75 75 80 8 8 true;
  notation 80 53 55;
  init_setup 75 75 80

let conv_to_x coor = (coor - 75) / 80

let conv_to_y coor = 7 - conv_to_x coor

let conv_to_loc =
  let det_position =
    match wait_next_event [ Button_down ] with
    | { mouse_x; mouse_y } -> (mouse_x, mouse_y)
  in
  (conv_to_y (snd det_position), conv_to_x (fst det_position))

let draw_string_loc x y str =
  moveto x y;
  draw_string str

let promotion () =
  board_background 75 75 640 (rgb 0 0 0);
  board_background 315 315 160 (rgb 255 255 255);
  moveto 310 520;
  draw_string "Select a piece to promote to!";
  set_color black;
  draw_string_loc 335 355 "Queen";
  draw_string_loc 415 355 "Rook";
  draw_string_loc 335 435 "Bishop";
  draw_string_loc 415 435 "Knight"

(*let x = promotion ()*)
