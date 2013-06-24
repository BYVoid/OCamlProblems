(* Construct the bottom-up order sequence of the tree nodes
Write a function bottom_up t which constructs the bottom-up sequence of the nodes of the multiway tree t. *)

type 'a mult_tree = T of 'a * 'a mult_tree list

let bottom_up tree =
  let rec bottom_up' acc tree =
    let T (elem, subtrees) = tree in
    List.fold_right (fun tree acc ->
      bottom_up' acc tree
    ) subtrees (elem :: acc)
  in
  bottom_up' [] tree

let tree = T('a', [T('f',[T('g',[])]); T('c',[]); T('b',[T('d',[]); T('e',[])])])

let () =
  let botton_up_str = bottom_up tree in
  List.iter (Printf.printf "%c ") botton_up_str;
  Printf.printf "\n";
  assert (botton_up_str = ['g'; 'f'; 'c'; 'd'; 'e'; 'b'; 'a'])
