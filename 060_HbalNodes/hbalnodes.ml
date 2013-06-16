(* Construct height-balanced binary trees with a given number of nodes
Consider a height-balanced binary tree of height h. What is the maximum number of nodes it can contain? Clearly, maxN = 2h - 1. However, what is the minimum number minN? This question is more difficult. Try to find a recursive statement and turn it into a function minNodes defined as follows: minNodes h returns the minimum number of nodes in a height-balanced binary tree of height h.

Solution
On the other hand, we might ask: what is the maximum height H a height-balanced binary tree with N nodes can have? maxHeight n returns the maximum height of a height-balanced binary tree with n nodes.

Now, we can attack the main problem: construct all the height-balanced binary trees with a given nuber of nodes. hbal_tree_nodes n returns a list of all height-balanced binary tree with n nodes.

Find out how many height-balanced trees exist for n = 15. *)

type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree

let rec minNodes h =
  if h = 1 then
    1
  else if h = 2 then
    2
  else
    (minNodes (h - 1)) + (minNodes (h - 2)) + 1

let maxHeight num_nodes =
  let rec find height =
    if minNodes (height + 1) > num_nodes then
      height
    else
      find (height + 1)
  in
  if num_nodes = 0 then
    0
  else
    find 1

let rec calc_nodes = function
  | Empty -> 0
  | Node (_, left, right) -> (calc_nodes left) + (calc_nodes right) + 1

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

let rec hbal_tree_nodes num_nodes =
  if num_nodes = 0 then
    [Empty]
  else
    let max_height = maxHeight num_nodes in
    let min_height = max_height - 1 in
    let trees = (hbal_tree max_height) @ (hbal_tree min_height) in
    List.filter (fun tree -> calc_nodes tree = num_nodes) trees

let () =
  Printf.printf "%d\n" (List.length (hbal_tree_nodes 15))
