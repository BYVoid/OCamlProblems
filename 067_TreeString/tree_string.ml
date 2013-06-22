(*
A string representation of binary trees

Somebody represents binary trees as strings of the following type (see example):
  "a(b(d,e),c(,f(g,)))".

Write an OCaml function which generates this string representation, if the tree is given
  as usual (as Empty or Node(x,l,r) term). Then write a function which does this inverse;
  i.e. given the string representation, construct the tree in the usual form. Finally,
  combine the two predicates in a single function tree_string which can be used in both
  directions.

Write the same predicate tree_string using difference lists and a single predicate
  tree_dlist which does the conversion between a tree and a difference list in both
  directions.

For simplicity, suppose the information in the nodes is a single letter and there are no
  spaces in the string.
*)

type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree

let rec tree_string = function
  | Empty -> ""
  | Node (x, Empty, Empty) -> Char.escaped x
  | Node (x, left, right) ->
    (Char.escaped x) ^ "(" ^ tree_string left ^ "," ^ tree_string right ^ ")"

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
  let result = tree_string tree in
  Printf.printf "%s\n" result
