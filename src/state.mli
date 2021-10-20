(** Represents the state of the chess game.

    The module represents the state of a chess game. *)

type t
(** Abstract type representing state of a chess game. State includes
    what piece is on a grid in the board.*)

exception InvalidLocation of (int * int)

val init_state : Board.t -> t
(** [init_state b] is the initial state of board [b]. *)

val what_piece : t -> int * int -> Pieces.piece
(**[what_piece st l] is the piece at location [l] in state [st]. Raises
   [InvalidLocation l] if [l] is not in the board. *)

val update_loc : t -> int * int -> Pieces.piece -> t
(** [update_loc st l p] is state [st] updated so that the piece at
    location [l] is [p]. *)

(* NOTE: To completely update the state after a move on the board, you
   have to call update_loc twice - once to set the destination location
   to a new piece and again to set the original location to a none piece
   (or whatever should be in the original location). Alternatively, call
   init_state on the updated board.*)
