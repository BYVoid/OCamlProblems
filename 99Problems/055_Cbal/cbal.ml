(* Construct completely balanced binary trees
In a completely balanced binary tree, the following property holds for every node: The number of nodes in its left subtree and the number of nodes in its right subtree are almost equal, which means their difference is not greater than one.

Write a function cbal_tree to construct completely balanced binary trees for a given number of nodes. The function should generate all solutions via backtracking. Put the letter 'x' as information into all nodes of the tree. *)

type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree

let rec cbal_tree num_nodes =
  let cons left right acc =
    List.fold_left (fun acc node_left ->
      List.fold_left (fun acc node_right ->
        let new_node = Node ('x', node_left, node_right) in
        new_node :: acc
      ) acc right
    ) acc left
  in
  if num_nodes = 0 then
    [Empty]
  else if num_nodes mod 2 = 1 then
    let sub = cbal_tree (num_nodes / 2) in
    cons sub sub []
  else
    let sub1 = cbal_tree (num_nodes / 2) in
    let sub2 = cbal_tree (num_nodes / 2 - 1) in
    cons sub2 sub1 (cons sub1 sub2 [])

let () =
  for i = 1 to 50 do
    let tree = cbal_tree i in
    Printf.printf "%d %d\n%!" i (List.length tree);
  done;
