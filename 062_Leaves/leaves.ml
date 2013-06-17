(* Collect the leaves of a binary tree in a list
A leaf is a node with no successors. Write a function leaves to collect them in a list. *)

type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree

let (|>) x f = f x

let leaves tree =
  let rec leaves' tree acc = match tree with
    | Empty -> acc
    | Node (elem, Empty, Empty) -> elem :: acc
    | Node (_, left, right) -> leaves' left acc |> leaves' right
  in
  List.rev (leaves' tree [])

let () =
  let example_tree =
    Node('a', Node('b', Node('d', Empty, Empty), Node('e', Empty, Empty)),
         Node('c', Empty, Node('f', Node('g', Empty, Empty), Empty))) in
  if leaves example_tree = ['d'; 'e'; 'g'] then
    Printf.printf "Yes\n"
