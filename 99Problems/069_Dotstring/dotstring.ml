(* Dotstring representation of binary trees
   We consider again binary trees with nodes that are identified by single lower-case
   letters, as in the example of problem “A string representation of binary trees”.
   Such a tree can be represented by the preorder sequence of its nodes in which dots (.)
   are inserted where an empty subtree (nil) is encountered during the tree traversal. For
   example, the tree shown in problem “A string representation of binary trees” is
   represented as 'abd..e..c.fg...'. First, try to establish a syntax (BNF or syntax
   diagrams) and then write a function tree_dotstring which does the conversion in both
   directions. Use difference lists. *)

type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree

let rec traverse empty f = function
  | Empty -> empty
  | Node (elem, left, right) -> f elem (traverse empty f left) (traverse empty f right)

let tree_dotstring tree =
  traverse "." (fun elem left right -> (Char.escaped elem) ^ left ^ right) tree

let dotstring_tree str =
  let rec gen index =
    let ch = str.[index] in
    if ch = '.' then
      index, Empty
    else
      let index, left = gen (index + 1) in
      let index, right = gen (index + 1) in
      index, Node (ch, left, right)
  in
  snd (gen 0)

let tree =
  Node ('n',
    Node ('k',
      Node ('c',
        Node ('a',
          Empty,
          Empty
        ),
        Node ('e',
          Node ('d',
            Empty,
            Empty
          ),
          Node ('g',
            Empty,
            Empty
          )
        )
      ),
      Node ('m',
        Empty,
        Empty
      )
    ),
    Node ('u',
      Node ('p',
        Empty,
        Node ('q',
          Empty,
          Empty
        )
      ),
      Empty
    )
  )

let () =
  let dotstring = tree_dotstring tree in
  let reconstructed = dotstring_tree dotstring in
  Printf.printf "%s\n" dotstring;
  if reconstructed = tree then
    Printf.printf "Yes\n"
