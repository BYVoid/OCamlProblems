(* Find the k'th element of a list. *)

let rec at pos = function
  | [] -> None
  | x::tail -> if pos = 1 then Some x else at (pos - 1) tail

let () =
  let result = at 2 [1;2;3;4] in
  match result with
    | None -> Printf.printf "Invalid index\n"
    | Some x -> Printf.printf "%i\n" x
