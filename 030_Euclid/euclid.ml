(* Determine the greatest common divisor of two positive integer numbers.
Use Euclid's algorithm. *)

let rec gcd x y =
  let remainder = x mod y in
  if remainder = 0 then
    y
  else
    gcd y remainder

let () =
  Printf.printf "%d\n" (gcd 128 144)
