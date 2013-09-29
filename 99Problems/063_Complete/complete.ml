(* Construct a complete binary tree
A complete binary tree with height H is defined as follows: The levels 1,2,3,...,H-1 contain the maximum number of nodes (i.e 2i-1 at the level i, note that we start counting the levels from 1 at the root). In level H, which may contain less than the maximum possible number of nodes, all the nodes are "left-adjusted". This means that in a levelorder tree traversal all internal nodes come first, the leaves come second, and empty successors (the nil's which are not really nodes!) come last.

Particularly, complete binary trees are used as data structures (or addressing schemes) for heaps.

We can assign an address number to each node in a complete binary tree by enumerating the nodes in levelorder, starting at the root with number 1. In doing so, we realize that for every node X with address A the following property holds: The address of X's left and right successors are 2*A and 2*A+1, respectively, supposed the successors do exist. This fact can be used to elegantly construct a complete binary tree structure. Write a function is_complete_binary_tree with the following specification: is_complete_binary_tree n t returns true iff t is a complete binary tree with n nodes. *)

type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree

let complete_binary_tree num_nodes =
  let rec cons num =
    let left = num * 2 in
    let right = left + 1 in
    if right <= num_nodes then
      Node (num, cons left, cons right)
    else if left <= num_nodes then
      Node (num, cons left, Empty)
    else
      Node (num, Empty, Empty)
  in
  if num_nodes = 0 then Empty else cons 1

let rec count_nodes = function
  | Empty -> 0
  | Node (_, left, right) -> count_nodes left + count_nodes right + 1

let is_complete_binary_tree num_nodes tree =
  let rec test num node =
    let left_num = num * 2 in
    let right_num = left_num + 1 in
    match node with
      | Empty -> num_nodes = 0
      | Node (_, Empty, Empty) -> left_num > num_nodes
      | Node (_, left, Empty) -> right_num > num_nodes && test left_num left
      | Node (_, Empty, right) -> false
      | Node (_, left, right) -> test left_num left && test right_num right
  in
  count_nodes tree = num_nodes && test 1 tree


let () =
  let example_tree = Node (1, Node (2, Node (4, Empty, Empty), Node (5, Empty, Empty)), Node (3, Empty, Empty)) in
  if complete_binary_tree 5 = example_tree && is_complete_binary_tree 5 example_tree then
    Printf.printf "Yes\n"
