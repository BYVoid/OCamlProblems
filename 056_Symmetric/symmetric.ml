(* Symmetric binary trees
Let us call a binary tree symmetric if you can draw a vertical line through the root node and then the right subtree is the mirror image of the left subtree. Write a function is_symmetric to check whether a given binary tree is symmetric.

Hint: Write a function is_mirror first to check whether one tree is the mirror image of another. We are only interested in the structure, not in the contents of the nodes. *)

type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree

let is_symmetric tree =
  let rec is_mirror t1 t2 = match t1, t2 with
    | Node(c1, l1, r1), Node(c2, l2, r2) ->
      c1 = c2 && is_mirror l1 r2 && is_mirror r1 l2
    | Empty, Empty -> true
    | _ -> false
  in
  match tree with
    | Empty -> true
    | Node(_, l, r) -> is_mirror l r

let () =
  if is_symmetric (Node (1, Empty, Empty)) then
  Printf.printf "Yes\n"
