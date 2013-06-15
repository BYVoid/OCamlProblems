(* Determine whether a given integer number is prime. *)

let is_prime num =
  let rec test divisor =
    if divisor * divisor > num then
      true
    else if num mod divisor = 0 then
      false
    else
      test (divisor + 1)
    in
  num >=2 && test 2

let () =
  for num = 2 to 1000 do
    if is_prime num then
      Printf.printf "%d " num
  done;
  Printf.printf "\n"
