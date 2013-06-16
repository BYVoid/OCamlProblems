(* Construct height-balanced binary trees
In a height-balanced binary tree, the following property holds for every node: The height of its left subtree and the height of its right subtree are almost equal, which means their difference is not greater than one.

Write a function hbal_tree to construct height-balanced binary trees for a given height. The function should generate all solutions via backtracking. Put the letter 'x' as information into all nodes of the tree. *)

type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree

let rec hbal_tree height =
  let cons left right acc =
    List.fold_left (fun acc node_left ->
      List.fold_left (fun acc node_right ->
        let new_node = Node ('x', node_left, node_right) in
        new_node :: acc
      ) acc right
    ) acc left
  in
  if height = 0 then
    [Empty]
  else if height = 1 then
    [Node ('x', Empty, Empty)]
  else
    let sub1 = hbal_tree (height - 1) in
    let sub2 = hbal_tree (height - 2) in
    let acc = cons sub1 sub1 [] in
    let acc = cons sub1 sub2 acc in
    let acc = cons sub2 sub1 acc in
    acc

let () =
  let x = 'x' in
  if hbal_tree 3 = [Node (x, Node (x, Empty, Empty), Node (x, Node (x, Empty, Empty), Node (x, Empty, Empty)));                             Node (x, Node (x, Empty, Empty), Node (x, Node (x, Empty, Empty), Empty));                                              Node (x, Node (x, Empty, Empty), Node (x, Empty, Node (x, Empty, Empty)));                                              Node (x, Node (x, Node (x, Empty, Empty), Node (x, Empty, Empty)), Node (x, Empty, Empty));                             Node (x, Node (x, Node (x, Empty, Empty), Empty), Node (x, Empty, Empty));                                              Node (x, Node (x, Empty, Node (x, Empty, Empty)), Node (x, Empty, Empty));                                              Node (x, Node (x, Node (x, Empty, Empty), Node (x, Empty, Empty)),                                                       Node (x, Node (x, Empty, Empty), Node (x, Empty, Empty)));                                                             Node (x, Node (x, Node (x, Empty, Empty), Node (x, Empty, Empty)), Node (x, Node (x, Empty, Empty), Empty));            Node (x, Node (x, Node (x, Empty, Empty), Node (x, Empty, Empty)), Node (x, Empty, Node (x, Empty, Empty)));            Node (x, Node (x, Node (x, Empty, Empty), Empty), Node (x, Node (x, Empty, Empty), Node (x, Empty, Empty)));            Node (x, Node (x, Node (x, Empty, Empty), Empty), Node (x, Node (x, Empty, Empty), Empty));                             Node (x, Node (x, Node (x, Empty, Empty), Empty), Node (x, Empty, Node (x, Empty, Empty)));                            
 Node (x, Node (x, Empty, Node (x, Empty, Empty)), Node (x, Node (x, Empty, Empty), Node (x, Empty, Empty)));
 Node (x, Node (x, Empty, Node (x, Empty, Empty)), Node (x, Node (x, Empty, Empty), Empty));
 Node (x, Node (x, Empty, Node (x, Empty, Empty)), Node (x, Empty, Node (x, Empty, Empty)))] then
    Printf.printf "Yes\n"
