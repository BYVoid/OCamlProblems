(* Collect the internal nodes of a binary tree in a list
An internal node of a binary tree has either one or two non-empty successors. Write a function internals to collect them in a list. *)

type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree

let (|>) x f = f x

let internal tree =
  let rec internal' tree acc = match tree with
    | Empty | Node (_, Empty, Empty) -> acc
    | Node (elem, left, right) -> (elem :: acc) |> internal' left |> internal' right
  in
  List.rev (internal' tree [])

let () =
  let example_tree =
    Node('a', Node('b', Node('d', Empty, Empty), Node('e', Empty, Empty)),
         Node('c', Empty, Node('f', Node('g', Empty, Empty), Empty))) in
  if internal example_tree = ['a'; 'b'; 'c'; 'f'] then
    Printf.printf "Yes\n"
