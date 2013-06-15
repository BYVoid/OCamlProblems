(* A list of prime numbers.
Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range. *)

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

let all_primes lower upper =
  let rec find current acc =
    if current >= upper then
      acc
    else if is_prime current then
      find (current + 1) (current :: acc)
    else
      find (current + 1) acc 
    in
  List.rev (find lower [])

let () =
  List.iter (Printf.printf "%d ") (all_primes 2 7920);
  Printf.printf "\n"
