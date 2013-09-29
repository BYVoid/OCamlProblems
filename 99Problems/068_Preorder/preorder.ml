(*Preorder and inorder sequences of binary trees
We consider binary trees with nodes that are identified by single lower-case letters, as
  in the example of the previous problem.

Write functions preorder and inorder that construct the preorder and inorder sequence of a
  given binary tree, respectively. The results should be atoms, e.g. 'abdecfg' for the
  preorder sequence of the example in the previous problem.
Can you use preorder from problem part 1. in the reverse direction; i.e. given a preorder
  sequence, construct a corresponding tree? If not, make the necessary arrangements.
If both the preorder sequence and the inorder sequence of the nodes of a binary tree are
  given, then the tree is determined unambiguously. Write a function pre_in_tree that does
  the job.
Solve problems 1. to 3. using difference lists. Cool! Use the function timeit (defined in
  problem “Compare the two methods of calculating Euler's totient function.”) to compare
  the solutions.
What happens if the same character appears in more than one node. Try for instance
  pre_in_tree "aba" "baa". *)

type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree

let rec traverse empty f = function
  | Empty -> empty
  | Node (elem, left, right) -> f elem (traverse empty f left) (traverse empty f right)

let preorder tree =
  traverse "" (fun elem left right -> (Char.escaped elem) ^ left ^ right) tree

let inorder tree =
  traverse "" (fun elem left right -> left ^ (Char.escaped elem) ^ right) tree

let rec build preorder inorder =
  let len = String.length preorder in
  if len = 0 then
    Empty
  else
    let elem = String.get preorder 0 in
    let mid_pos = String.index inorder elem in
    let left_inorder = String.sub inorder 0 mid_pos in
    let right_inorder = String.sub inorder (mid_pos + 1) (len - mid_pos - 1) in
    let left_preorder = (String.sub preorder 1 mid_pos) in
    let right_preorder =  (String.sub preorder (mid_pos + 1) (len - mid_pos - 1)) in
    let left = build left_preorder left_inorder in
    let right = build right_preorder right_inorder in
    Node (elem, left, right)

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
  let preorder_str = preorder tree in
  let inorder_str = inorder tree in
  Printf.printf "Preorder: %s\n" preorder_str;
  Printf.printf "Inorder: %s\n" inorder_str;
  let reconstructed = build preorder_str inorder_str in
  if reconstructed = tree then
    Printf.printf "Yes\n"
