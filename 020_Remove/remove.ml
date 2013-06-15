(* Remove the K'th element from a list.
The first element of the list is numbered 0, the second 1,... *)

let remove_at pos lst =
  let rec iter count acc = function
    | elem :: tail ->
      if count < pos then
        iter (count + 1) (elem::acc) tail
      else
        acc @ tail
    | [] -> acc in
  iter 0 [] lst

let () =
  let result = remove_at 1 [`a;`b;`c;`d] = [`a;`c;`d] in
  if result then
    Printf.printf "Yes\n"
