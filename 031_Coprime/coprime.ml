(* Two numbers are coprime if their greatest common divisor equals 1. *)

let rec gcd x y =
  let remainder = x mod y in
  if remainder = 0 then
    y
  else
    gcd y remainder

let coprime x y =
  (gcd x y) = 1

let () =
  if coprime 128 144 then
    Printf.printf "Yes\n"
  else
    Printf.printf "No\n"
