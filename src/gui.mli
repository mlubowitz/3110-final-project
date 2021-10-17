(* Functions for graphical user interface *)

val color_rec : int -> int -> int -> bool -> unit;;
(** [color_rec] draws a rectangle that is either dark or light *)

val draw_matrix : int -> int -> int -> int -> int -> bool -> unit;;
(** [draw_matrix_rows] draws a matrix with a specified number of rows and columns *)

val init : unit;;
(** [init] creates a window and initializes board*)