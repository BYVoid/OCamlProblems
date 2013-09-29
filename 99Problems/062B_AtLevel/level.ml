(* Collect the nodes at a given level in a list.
A node of a binary tree is at level N if the path from the root to the node has length N-1. The root node is at level 1. Write a function at_level t l to collect all nodes of the tree t at level l in a list. *)

type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree

let (|>) x f = f x

let at_level tree level =
  let rec at_level' tree depth acc = match tree with
    | Empty -> acc
    | Node (elem, left, right) ->
      if depth = level then
        elem :: acc
      else
        acc |> at_level' left (depth + 1) |> at_level' right (depth + 1)
  in
  List.rev (at_level' tree 1 [])

let () =
  let example_tree =
    Node('a', Node('b', Node('d', Empty, Empty), Node('e', Empty, Empty)),
         Node('c', Empty, Node('f', Node('g', Empty, Empty), Empty))) in
  if (at_level example_tree 2 = ['b'; 'c']) && (at_level example_tree 5 = []) then
    Printf.printf "Yes\n"
