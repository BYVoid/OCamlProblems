(* Construct a list containing the prime factors and their multiplicity. *)

let factors num =
  let rec divide num divisor acc =
    if num = 1 then
      acc
    else if num mod divisor = 0 then
      let new_acc = match acc with
        | (head_divisor, freq) :: tail ->
          if head_divisor = divisor then (head_divisor, freq + 1) :: tail else (divisor, 1) :: acc
        | [] -> (divisor, 1) :: acc
        in
      divide (num / divisor) divisor new_acc
    else
      divide num (divisor + 1) acc
    in
  List.rev (divide num 2 [])

let () =
  List.iter (Printf.printf "%d ") (factors 315);
  Printf.printf "\n"
