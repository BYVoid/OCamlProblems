(* Determine the prime factors of a given positive integer.
Construct a flat list containing the prime factors in ascending order. *)

let factors num =
  let rec divide num divisor acc =
    if num = 1 then
      acc
    else if num mod divisor = 0 then
      divide (num / divisor) divisor (divisor :: acc)
    else
      divide num (divisor + 1) acc
    in
  List.rev (divide num 2 [])

let () =
  List.iter (Printf.printf "%d ") (factors 315);
  Printf.printf "\n"
