type piece = Pawn | Rook | Knight | Bishop | Queen | King

exception Illegal

let piece_move (p:piece) (s:(*square type*)) : (*new matrix*) = raise Illegal

let is_legal (p:piece) (s:(*square type*)) : (b:bool) = raise Illegal