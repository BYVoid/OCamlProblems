(* Generate-and-test paradigm
Apply the generate-and-test paradigm to construct all symmetric, completely balanced binary trees with a given number of nodes. *)

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

let sym_cbal_trees n =
  List.filter is_symmetric (cbal_tree n)

let () =
  Printf.printf "%d\n" (List.length (sym_cbal_trees 57))
