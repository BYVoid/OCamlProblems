(* Create a list containing all integers within a given range.
If first argument is smaller than second, produce a list in decreasing order. *)

let range first last =
  let rec iter current last acc =
    if current < last then
      iter (current + 1) last (current :: acc)
    else
      current :: acc
    in
  if first < last then
    List.rev (iter first last [])
  else
    iter last first []

let () =
  let result = (range 4 9 = [4;5;6;7;8;9]) && (range 9 4 = [9;8;7;6;5;4]) in
  if result then
    Printf.printf "Yes\n"
