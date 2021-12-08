type t = string array array

type grid = int * int

(* [right_piece row col color] is the piece that is in [row] and [col]
   in the starting chess board. Uppercase is black. Requires: [row and
   col are ints in range 0..7].*)
let right_piece row col color =
  let str_piece =
    match row with
    | 1
    | 6 ->
        "P"
    | 0
    | 7 -> begin
        match col with
        | 0
        | 7 ->
            "R"
        | 1
        | 6 ->
            "N"
        | 2
        | 5 ->
            "B"
        | 3 -> "Q"
        | 4 -> "K"
        | _ -> failwith "Precondition violated"
      end
    | _ -> failwith "Precondition violated"
  in
  if color = "black" then "[" ^ str_piece ^ "]"
  else "[" ^ String.lowercase_ascii str_piece ^ "]"

let init_board () =
  let b = Array.make_matrix 8 8 "[ ]" in
  let () =
    for row = 0 to 7 do
      for col = 0 to 7 do
        if row = 0 || row = 1 then
          b.(row).(col) <- right_piece row col "black"
        else if row = 6 || row = 7 then
          b.(row).(col) <- right_piece row col "white"
        else b.(row).(col) <- "[ ]"
      done
    done
  in
  Array.copy b

(* ============= move_piece is below =================================*)

let move_piece board ori_loc new_loc =
  let n_row = fst new_loc in
  let n_col = snd new_loc in
  let ori_row = fst ori_loc in
  let ori_col = snd ori_loc in
  let () = board.(n_row).(n_col) <- board.(ori_row).(ori_col) in
  let () = board.(ori_row).(ori_col) <- "[ ]" in
  Array.copy board

(* ============= flip is below ======================================*)

(* [flip_row row] is an Array with length 8 that is a copy of [row]
   reversed. This is a helper to [flip].*)
let flip_row row =
  let new_ar_row = Array.make 8 "" in
  let () =
    for x = 0 to 7 do
      new_ar_row.(x) <- row.(7 - x)
    done
  in
  Array.copy new_ar_row

let flip board =
  (* need to reverse everything in row and reverse order of arrays in
     array *)
  let fl_brd = Array.make_matrix 8 8 "" in
  let () =
    for x = 0 to 7 do
      fl_brd.(x) <- flip_row board.(7 - x)
    done
  in
  Array.copy fl_brd

(* =========== print_board is below ==================================*)

(* [get_row_str row last_ind] is a string version of [row]. This is a
   helper to [ar_to_str].*)
let rec get_row_str row last_ind =
  (* board.(row) is the array that we are turning into string *)
  match last_ind with
  | -1 -> " "
  | x -> get_row_str (Array.sub row 0 x) (x - 1) ^ row.(x)

(* [ar_to_str board str_board] mutates str_board so that it contains the
   string versions of the array rows that were in [board]. This is a
   helper to [print_board]*)
let ar_to_str board str_board =
  for x = 0 to 7 do
    str_board.(x) <- get_row_str board.(x) 7
    (*7 (last arg of get_row_str) is the index of last element*)
  done

let print_board board =
  let str_board = Array.make 8 "" in
  let () = ar_to_str board str_board in
  for x = 0 to 7 do
    print_endline str_board.(x)
  done

(* ==================get_str_piece======================================== *)

let get_str_piece board loc =
  match loc with
  | x, y -> board.(x).(y)

(* ==================get_row======================================== *)

let get_row board row =
  let rec make_lst ind =
    match ind with
    | 8 -> []
    | x -> board.(row).(x) :: make_lst (x + 1)
  in
  make_lst 0

let promote_piece board ori_loc new_loc (piece_type : string) =
  let n_row = fst new_loc in
  let n_col = snd new_loc in
  let ori_row = fst ori_loc in
  let ori_col = snd ori_loc in
  let () = board.(n_row).(n_col) <- "[" ^ piece_type ^ "]" in
  let () = board.(ori_row).(ori_col) <- "[ ]" in
  Array.copy board

(* ==================all_boards======================================== *)
