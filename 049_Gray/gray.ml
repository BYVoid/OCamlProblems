(* Gray code.
An n-bit Gray code is a sequence of n-bit strings constructed according to certain rules. For example,

n = 1: C(1) = ['0','1'].
n = 2: C(2) = ['00','01','11','10'].
n = 3: C(3) = ['000','001','011','010',´110´,´111´,´101´,´100´].
Find out the construction rules and write a function with the following specification: gray n returns the n-bit Gray code. *)

let bin_to_string num_bits num =
  let rec gen num pos acc =
    let bit = if num mod 2 = 0 then "0" else "1" in
    if pos < num_bits then
      gen (num / 2) (pos + 1) (bit ^ acc)
    else
      acc
  in
  gen num 0 ""

let rec pow num p =
  if p = 0 then
    1
  else
    let sub = pow num (p / 2) in
    if p mod 2 = 0 then
      sub * sub
    else
      num * sub * sub

let gray num_bits =
  let rec gen num acc =
    if num = 0 then
      num :: acc
    else
      gen (num - 1) (num :: acc)
  in
  let code = List.map (fun num -> num lxor (num lsr 1)) (gen ((pow 2 num_bits) - 1) []) in
  List.map (bin_to_string num_bits) code

let () =
  let num_bits = 3 in
  let results = gray num_bits in
  List.iter (Printf.printf "%s ") results;
  Printf.printf "\n";
