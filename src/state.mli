(** Represents the state of the chess game.

    The module represents the state of a chess game. *)

type t
(** Abstract type representing state of a chess game. State includes
    what piece is on a grid in the board.*)

exception InvalidLocation of (int * int)

val get_state : Board.t -> t
(** [get_state b] is the state of board [b]. *)

val what_piece : t -> int * int -> Pieces.piece
(**[what_piece st l] is the piece at location [l] in state [st]. Raises
   [InvalidLocation l] if [l] is not in the board. *)

val is_path_empty : t -> int * int -> int * int -> bool

(* Update function is unnecessary - just call get_state *)
(* val update_loc : t -> int * int -> Pieces.piece -> t (** [update_loc
   st l p] is state [st] updated so that the piece at location [l] is
   [p]. *) *)
