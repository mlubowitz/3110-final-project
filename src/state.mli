(** Represents the state of the chess game.

    The module represents the state of a chess game. *)

type t
(** Abstract type representing state of a chess game. State includes
    what piece is on a grid in the board.*)

exception InvalidLocation of (int * int)

val init_state : Board.t -> t
(** [get_state b] is the state of board [b]. *)

val what_piece : t -> int * int -> Pieces.piece
(**[what_piece st l] is the piece at location [l] in state [st]. Raises
   [InvalidLocation l] if [l] is not in the board. *)

val is_en_passant : t -> Pieces.piece -> Pieces.piece -> bool
(**[is_en_passant st p p2] is [true] if p can capture p2 by en passant*)

val promotion : t -> Pieces.piece -> int * int -> bool
(**[promotion st p pdest] is true if p is a [Pawn] and it is moved to
   the first row*)

val update_loc : t -> int * int -> Pieces.piece -> t
(** [update_loc st l p] is state [st] updated so that the piece at
    location [l] is piece [p]. If there is a piece that was already at
    location [l], that piece is replaced by the new piece. The original
    location of piece [p] is set to an empty grid (None piece).*)

val update_board :
  Board.t -> t -> int * int -> int * int -> bool -> bool -> Board.t

val is_path_empty : t -> int * int -> int * int -> bool
(**[is_path_empty st l1 l2] is true if path from location [l1] to [l2]
   on the chess board has no pieces, false otherwise. *)

val flip_state : t -> t
(**[flip_state st] is st but with the locations of pieces updated so
   that the state reflects the flipped board. *)

val castle_side : t -> Pieces.piece -> Pieces.piece
(**[castle_side st p2] is the rook the king wants to castle with given
   that the king wants to move to [p2]*)

val castle_allowed : t -> int * int -> int * int -> bool
(** [castle_allowed state king_loc king_dest] is true if a castle move
    is allowed for the king at location [king_loc] and the dest loc
    [king_dest]. Requries: [king_loc is location of a king] *)

val is_legal_castle : t -> Pieces.piece -> Pieces.piece -> bool
(**[is_legal_castle st p p2] is [true] if the king can castle moving
   from its location at p to the location of p2 and [false] otherwise*)

val in_check : t -> Pieces.piece -> bool
(**[in_check st p] returns [true] if the king is in check *)

val find_king : t -> string -> int * int
(** [fing_king st c] is the location of the king with color [c] in state
    [st]. Colors are "W", "B", or "N" (no color).*)

val can_king_move : t -> Pieces.piece -> bool
(**[can_piece_move st p] is true if a piece has any legal moves on the
   board*)

val checkpath_list : t -> Pieces.piece -> (int * int) list
(**[checkpath_list st p] is the list of all positions on the board
   between the king and the piece that is putting it in check, including
   the position of the piece giving the check*)

val state_to_list : t -> ((int * int) * Pieces.piece) list
(**[state_to_list t] is a list of the list given by t*)

val st_with_two_pces : int * int -> t
(**[st_with_two_pces loc] is a state with only the piece at location
   [loc] and the white king at (7,4). All other spots on the board are
   empty. This is used for testing in_check. The two pieces are in their
   normal initial positions. *)

val reset_en_passant : t -> string -> t
(**[reset_en_passant st color] is st but with all the en_passant values
   of the pieces of the color that did not just move set to false *)

val update_en_passant : int * int -> Pieces.piece -> Pieces.piece
(**[update_en_passant st color] is st but with all the en_passant values
   of the pieces of the color that did not just move set to false *)

val is_legal : t -> Pieces.piece -> Pieces.piece -> bool
(** [is_legal st p p2] is the legality of the movement of a given piece
    [p] from [ori_loc] to [new_loc]. Raises [Illegal s] if there is no
    piece at [ori_loc]*)

val insufficient_material : t -> bool
(**[insufficient_material st] is true if the pieces on the board are not
   sufficient for either player to achieve a checkmate, thus it is a
   stalemate*)

val piece_in_path : t -> int * int -> int * int -> Pieces.piece
(**[piece_in_path st loc1 loc2] returns the piece between [loc1] and
   [loc2] that is closest to loc1 if such as piece exists. If no piece
   in between pieces at [loc1] and [loc2] then return the piece at
   [loc2]. Never returns piece at [loc1]*)
