(* Binary search trees (dictionaries)
Construct a binary search tree from a list of integer numbers. *)

type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree

let is_symmetric tree =
  let rec is_mirror t1 t2 = match t1, t2 with
    | Node(c1, l1, r1), Node(c2, l2, r2) ->
      is_mirror l1 r2 && is_mirror r1 l2
    | Empty, Empty -> true
    | _ -> false
  in
  match tree with
    | Empty -> true
    | Node(_, l, r) -> is_mirror l r

let rec insert tree element = match tree with
  | Empty -> Node (element, Empty, Empty)
  | Node (current, left, right) ->
    if element <= current then
      Node (current, (insert left element), right)
    else
      Node (current, left, (insert right element))

let construct numbers =
  List.fold_left (fun tree element -> insert tree element) Empty numbers

let () =
  if is_symmetric(construct [5;3;18;1;4;12;21]) then
  Printf.printf "Yes\n"
