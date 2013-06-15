(* Insert an element at a given position into a list.
Start counting list elements with 0. *)

let insert_at new_elem pos lst =
  let rec iter count acc = function
    | elem :: tail ->
      if count < pos then
        iter (count + 1) (elem::acc) tail
      else
        acc @ (new_elem :: elem :: tail)
    | [] -> acc in
  iter 0 [] lst

let () =
  let result = insert_at `alfa 1 [`a;`b;`c;`d] = [`a;`alfa;`b;`c;`d] in
  if result then
    Printf.printf "Yes\n"
