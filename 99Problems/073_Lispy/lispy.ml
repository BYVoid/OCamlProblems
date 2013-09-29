(* Lisp-like tree representation
There is a particular notation for multiway trees in Lisp. The following pictures show how multiway tree structures are represented in Lisp.

Note that in the "lispy" notation a node with successors (children) in the tree is always the first element in a list, followed by its children. The "lispy" representation of a multiway tree is a sequence of atoms and parentheses '(' and ')'. This is very close to the way trees are represented in OCaml, except that no constructor T is used. Write a function lispy : char mult_tree -> string that returns the lispy notation of the tree. *)

type 'a mult_tree = T of 'a * 'a mult_tree list

let rec lispy = function
  | T (elem, []) -> Char.escaped elem
  | T (elem, subtrees) ->
    List.fold_left (fun acc tree ->
      acc ^ " " ^ lispy tree
    ) ("(" ^ Char.escaped elem) subtrees ^ ")"

let tree = T('a', [T('f',[T('g',[])]); T('c',[]); T('b',[T('d',[]); T('e',[])])])

let () =
  let lispy_str = lispy tree in
  Printf.printf "%s\n" lispy_str;
  assert (lispy_str = "(a (f g) c (b d e))")
