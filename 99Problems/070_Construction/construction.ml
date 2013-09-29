(* Tree construction from a node string
We suppose that the nodes of a multiway tree contain single characters. In the depth-first
   order sequence of its nodes, a special character ^ has been inserted whenever, during
   the tree traversal, the move is a backtrack to the previous level.

By this rule, the tree in the figure opposite is represented as: afg^^c^bd^e^^^.

Write functions string_of_tree : char mult_tree -> string to construct the string
   representing the tree and tree_of_string : string -> char mult_tree to construct the
   tree when the string is given. *)

type 'a mult_tree = T of 'a * 'a mult_tree list

let rec string_of_tree (T (elem, subtrees)) =
  let sub = List.fold_left (fun acc tree ->
    acc ^ (string_of_tree tree)
  ) "" subtrees in
  Char.escaped elem ^ sub ^ "^"

let get_equal lst tier =
  let rec get yes no = function
    | [] -> yes, no
    | (elem_tier, elem) as head :: tail ->
      if elem_tier = tier then get (elem :: yes) no tail else get yes (head :: no) tail
  in
  get [] [] lst

let tree_of_string str =
  let len = String.length str in
  let rec gen index tier nodes subtrees =
    if index < len then
      let elem = str.[index] in
      if elem = '^' then
        let elem = List.hd nodes in
        let left_nodes = List.tl nodes in
        let current_subtree, left_subtree = get_equal subtrees tier in
        let new_subtrees = (tier - 1), T (elem, current_subtree) in
        gen (index + 1) (tier - 1) left_nodes (new_subtrees :: List.rev left_subtree)
      else
        gen (index + 1) (tier + 1) (elem :: nodes) subtrees
    else
      snd (List.hd subtrees)
  in
  gen 0 0 [] []

let tree = T('a', [T('f',[T('g',[])]); T('c',[]); T('b',[T('d',[]); T('e',[])])])

let () =
  let str = string_of_tree tree in
  Printf.printf "%s\n" str;
  assert (tree_of_string str = tree)
