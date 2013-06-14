let rec last_two = function
  | [] | [_] -> None
  | x::[_] ->  Some x
  | _::tail -> last_two tail

let () =
  let result = last_two [1;2;3;4] in
  match result with
    | None -> Printf.printf "Empty list or only has 1 element\n"
    | Some x -> Printf.printf "%i\n" x
