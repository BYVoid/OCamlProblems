(* Find the number of elements of a list. *)

let length lst =
  let rec length acc = function
    | [] -> acc
    | _::tail -> length (acc + 1) tail
  in length 0 lst

let () =
  let result = length [1;2;3;4] in
  Printf.printf "%i\n" result
