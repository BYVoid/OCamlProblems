(* A list of Goldbach compositions.
Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.

In most cases, if an even number is written as the sum of two prime numbers, one of them is very small. Very rarely, the primes are both bigger than say 50. Try to find out how many such cases there are in the range 2..3000. *)

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

let goldbach num =
  let rec find one =
    let other = num - one in
    if (is_prime one) && (is_prime other) then
      (one, other)
    else
      find (one + 1)
    in
  find 2

let goldbach_list lower upper =
  let results = ref [] in
  for num = lower to upper do
    if (num > 2) && (num mod 2 = 0) then
      let a, b = goldbach num in
      results := (num, (a, b)) :: !results;
  done;
  List.rev !results

let goldbach_limit lower upper limit =
  List.filter (fun (_, (one, _)) -> one > limit) (goldbach_list lower upper)

let () =
  if goldbach_list 9 20
    = [(10, (3, 7)); (12, (5, 7)); (14, (3, 11)); (16, (3, 13)); (18, (5, 13));
     (20, (3, 17))] then
    Printf.printf "Yes\n";
  if goldbach_limit 1 2000 50
  = [(992, (73, 919)); (1382, (61, 1321)); (1856, (67, 1789));
     (1928, (61, 1867))] then
    Printf.printf "Yes\n"
