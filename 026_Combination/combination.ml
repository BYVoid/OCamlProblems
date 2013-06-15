(* Generate the combinations of K distinct objects chosen from the N elements of a list
In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients). For pure mathematicians, this result may be great. But we want to really generate all the possibilities in a list. *)

let combination count lst =
  let rec comb num (current: 'a list) (acc: 'a list list) = function
    | elem :: tail ->
      if num < count then
        let result = comb (num + 1) (elem :: current) acc tail in
        comb num current result tail
      else
        current :: acc
    | [] ->
      if num < count then
        acc
      else
        current :: acc
    in
  comb 0 [] [] lst

let () =
  let result = combination 2 [1;2;3;4] = [[4; 3]; [4; 2]; [3; 2]; [4; 1]; [3; 1]; [2; 1]] in
  if result then
    Printf.printf "Yes\n"
