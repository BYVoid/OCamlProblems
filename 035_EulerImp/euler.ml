(* Calculate Euler's totient function phi(m) (improved).
See problem “Calculate Euler's totient function phi(m)” for the definition of Euler's totient function. If the list of the prime factors of a number m is known in the form of the previous problem then the function phi(m) can be efficiently calculated as follows: Let [(p1, m1); (p2, m2); (p3, m3); ...] be the list of prime factors (and their multiplicities) of a given number m. Then φ(m) can be calculated with the following formula: *)

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

let rec pow n p =
  if p < 1 then 1 else n * pow n (p - 1);;

let phi_improved num =
  let fac = factors num in
  List.fold_left (fun acc (p, m) ->
    acc * (p - 1) * (pow p (m - 1))
  ) 1 fac

let () =
  for i = 1 to 20 do
    Printf.printf "%i: %i\n" i (phi_improved i)
  done;
  Printf.printf "\n"
