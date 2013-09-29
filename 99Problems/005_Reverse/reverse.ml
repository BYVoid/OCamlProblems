(* Reverse a list. *)

let reverse lst =
  let rec iter acc = function
    | [] -> acc
    | x::tail -> iter (x::acc) tail
  in iter [] lst

let () =
  let result = reverse [1;2;3;4] in
  List.iter (Printf.printf "%i ") result;
  Printf.printf "\n"
