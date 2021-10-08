(* Testing for final project *)

let rec make_row num_column =
  match num_column with 0 -> "" | _ -> "[]" ^ make_row (num_column - 1)

let rec print_rows num_rows row =
  match num_rows with
  | 0 -> print_endline "END OF BOARD"
  | x ->
      let () = print_endline row in
      print_rows (num_rows - 1) row

let rec print_game num_rows num_column =
  let row = make_row num_column in
  print_rows num_rows row

  (* WE NEED TO make matrix instead of using the above *)

let test_matrix = Array.make_matrix 8 8;;

let print_matrix m = print_endline m;;

