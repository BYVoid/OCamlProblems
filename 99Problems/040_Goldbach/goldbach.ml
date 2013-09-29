(* Goldbach's conjecture.
Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers. Example: 28 = 5 + 23. It is one of the most famous facts in number theory that has not been proved to be correct in the general case. It has been numerically confirmed up to very large numbers. Write a function to find the two prime numbers that sum up to a given even integer. *)

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

let () =
  for num = 4 to 50 do
    if num mod 2 = 0 then
      let a, b = goldbach num in
      Printf.printf "%d = %d + %d\n%!" num a b
  done
