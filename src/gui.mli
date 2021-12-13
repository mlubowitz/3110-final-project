(** Functions for graphical user interface *)

val color_rec : int -> int -> int -> bool -> unit
(** [color_rec] draws a rectangle that is either dark or light *)

val draw_matrix : int -> int -> int -> int -> int -> bool -> unit
(** [draw_matrix] draws a matrix with a specified number of rows and
    columns *)

val notation : int -> int -> int -> unit
(** [notation] creates the algebraic notation for the chess board
    ranging from 1-8 on the y-axis and A-H on the x-axis *)

val init_setup : int -> int -> int -> unit

(** [init_setup] initializes the chess board with all the pieaces *)

val init : unit
(** [init] creates a window and initializes board*)
