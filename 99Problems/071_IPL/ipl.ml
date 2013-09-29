(* Determine the internal path length of a tree
We define the internal path length of a multiway tree as the total sum of the path lengths from the root to all nodes of the tree. By this definition, the tree t in the figure of the previous problem has an internal path length of 9. Write a function ipl tree that returns the internal path length of tree. *)

type 'a mult_tree = T of 'a * 'a mult_tree list

let ipl tree =
  let rec ipl' depth tree =
    let T (_, subtrees) = tree in
    List.fold_left (fun acc tree ->
      acc + depth + ipl' (depth + 1) tree
    ) 0 subtrees
  in
  ipl' 1 tree

let tree = T('a', [T('f',[T('g',[])]); T('c',[]); T('b',[T('d',[]); T('e',[])])])

let () =
  Printf.printf "%d\n" (ipl tree)
