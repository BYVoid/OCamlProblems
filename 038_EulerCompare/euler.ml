(* Compare the two methods of calculating Euler's totient function.
Use the solutions of problems “Calculate Euler's totient function phi(m)” and “Calculate Euler's totient function phi(m) (improved)” to compare the algorithms. Take the number of logical inferences as a measure for efficiency. Try to calculate φ(10090) as an example. *)

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

let time f x =
  let t = Sys.time() in
  let fx = f x in
  Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
  fx

let () =
  let i = 10090 in
  ignore (time phi i);
  ignore (time phi_improved i)
