open Graphics  

type rectangle = string 


let color_rec x y s b = 
  draw_rect  x y s s;
  if b then set_color black else set_color (rgb 231 237 233);
  fill_rect x y s s;;

let rec draw_matrix_rows x y s row bl = 
match row with
| 1 -> color_rec x y s bl;
| _ -> color_rec x y s bl;
      draw_matrix_rows (x + s) (y) (s) (row - 1) (if bl then false else true);;

let rec draw_matrix x y s row col bl = 
match col with 
| 1 -> draw_matrix_rows x y s row bl;
| _ -> draw_matrix_rows x y s row bl ;
       draw_matrix x (y + s) s row (col - 1) (if bl then false else true);;
let init  = 
  open_graph " 800x800";
  set_window_title "Chess Game";
  draw_matrix 50 50 80 8 8 true;;
