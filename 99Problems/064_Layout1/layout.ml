(* 
Layout a binary tree (1)
As a preparation for drawing the tree, a layout algorithm is required to determine the position of each node in a rectangular grid. Several layout methods are conceivable, one of them is shown in the illustration below.


In this layout strategy, the position of a node v is obtained by the following two rules:

x(v) is equal to the position of the node v in the inorder sequence;
y(v) is equal to the depth of the node v in the tree.
In order to store the position of the nodes, we redefine the OCaml type representing a node (and its successors) as follows:

# type 'a pos_binary_tree =
    | E (* represents the empty tree *)
    | N of 'a * int * int * 'a pos_binary_tree * 'a pos_binary_tree;;
type 'a pos_binary_tree =
    E
  | N of 'a * int * int * 'a pos_binary_tree * 'a pos_binary_tree
N(w,x,y,l,r) represents a (non-empty) binary tree with root w "positioned" at (x,y), and subtrees l and r. Write a function layout_binary_tree with the following specification: layout_binary_tree t returns the "positioned" binary tree obtained from the binary tree t.
*)

type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree

type 'a pos_binary_tree =
  | E (* represents the empty tree *)
  | N of 'a * int * int * 'a pos_binary_tree * 'a pos_binary_tree

let layout_binary_tree tree =
  let rec layout_binary_tree' depth inorder = function
    | Empty -> inorder, E
    | Node (elem, left, right) ->
      let left_inorder, new_left = layout_binary_tree' (depth + 1) inorder left in
      let right_inorder, new_right = layout_binary_tree' (depth + 1) (left_inorder + 1) right in
      let pos_node = N (elem, left_inorder + 1, depth, new_left, new_right) in
      right_inorder, pos_node
  in
  snd (layout_binary_tree' 1 0 tree)

let tree =
  Node ('n',
    Node ('k',
      Node ('c',
        Node ('a',
          Empty,
          Empty
        ),
        Node ('h',
          Node ('g',
            Node ('e',
              Empty,
              Empty
            ),
            Empty
          ),
          Empty
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
        Node ('s',
          Node ('q',
            Empty,
            Empty
          ),
          Empty
        )
      ),
      Empty
    )
  )

let () =
  if layout_binary_tree tree = N ('n', 8, 1, N ('k', 6, 2, N ('c', 2, 3, N ('a', 1, 4, E, E), N ('h', 5, 4, N ('g', 4, 5, N ('e', 3, 6, E, E), E), E)), N ('m', 7, 3, E, E)), N ('u', 12, 2, N ('p', 9, 3, E, N ('s', 11, 4, N ('q', 10, 5, E, E), E)), E)) then
    Printf.printf "Yes\n"
