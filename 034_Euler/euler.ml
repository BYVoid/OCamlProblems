(* Calculate Euler's totient function phi(m).
Euler's so-called totient function φ(m) is defined as the number of positive integers r (1 ≤ r < m) that are coprime to m. We let φ(1) = 1.

Find out what the value of phi(m) is if m is a prime number. Euler's totient function plays an important role in one of the most widely used public key cryptography methods (RSA). In this exercise you should use the most primitive method to calculate this function (there are smarter ways that we shall discuss later). *)

let rec gcd x y =
  let remainder = x mod y in
  if remainder = 0 then
    y
  else
    gcd y remainder

let coprime x y =
  (gcd x y) = 1

let phi x =
  let res = ref 0 in
  for i = 1 to x - 1 do
    if coprime i x then
      res := !res + 1
  done;
  if x = 1 then 1 else !res

let () =
  for i = 1 to 20 do
    Printf.printf "%i: %i\n" i (phi i)
  done;
  Printf.printf "\n"
