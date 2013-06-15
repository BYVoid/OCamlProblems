(* Rotate a list N places to the left. *)

let split lst position =
  let rec iter count acc = function
    | elem::tail ->
      if count < position then
        iter (count + 1) (elem::acc) tail
      else
        (List.rev (elem::acc), tail)
    | [] -> (List.rev acc, []) in
  iter 1 [] lst

let rotate lst offset =
  let len = List.length lst in
  let (left, right) =
    if offset > 0 then
      split lst (offset mod len)
    else
      split lst (offset mod len + len)
    in
  right @ left

let () =
  let result = (rotate [`a;`b;`c;`d;`e;`f;`g;`h] 3 = [`d;`e;`f;`g;`h;`a;`b;`c])
    && (rotate [`a;`b;`c;`d;`e;`f;`g;`h] (-2) = [`g;`h;`a;`b;`c;`d;`e;`f]) in
  if result then
    Printf.printf "Yes\n"
