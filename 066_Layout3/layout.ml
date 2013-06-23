(* Yet another layout strategy is shown in the illustration below:
   The method yields a very compact layout while maintaining a certain symmetry in every
   node. Find out the rules and write the corresponding Prolog predicate. Hint: Consider
   the horizontal distance between a node and its successor nodes. How tight can you pack
   together two subtrees to construct the combined binary tree?

   Use the same conventions as in problem P64 and P65 and test your predicate in an
   appropriate way. Note: This is a difficult problem. Don't give up too early! *)

type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree

type 'a pos_binary_tree =
  | E (* represents the empty tree *)
  | N of 'a * int * int * 'a pos_binary_tree * 'a pos_binary_tree

let layout tree =
  let rec traverse depth = function
    | Empty -> (E, -1, -1)
    | Node (elem, left, right) ->
      let left, llspan, lrspan = traverse (depth + 1) left in
      let right, rlspan, rrspan = traverse (depth + 1) right in
      let branch_dist =
        if left = E || right = E then
          1 (* left or right is empty *)
        else
          (min lrspan rlspan) + 1 in
      let left_span =
        if left = E then
          0 (* left is empty *)
        else
          branch_dist + llspan in
      let right_span =
        if right = E then
          0 (* right is empty *)
        else
          branch_dist + rrspan in
      N (elem, left_span, branch_dist, left, right), left_span, right_span
  in
  let rec layout' offset depth = function
    | E -> E
    | N (elem, left_span, branch_dist, left, right) ->
      let pos = offset + left_span in
      let rlspan = match right with
        | E -> 0
        | N (_, left_span, _, _, _) -> left_span
      in
      let left = layout' offset (depth + 1) left in
      let right = layout' (pos + branch_dist - rlspan) (depth + 1) right in
      N (elem, pos, depth, left, right)
  in
  let span_tree, _, _ = traverse 1 tree in
  let pos_tree = layout' 1 1 span_tree in
  pos_tree

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
  if layout tree = N ('n', 5, 1, N ('k', 3, 2, N ('c', 2, 3, N ('a', 1, 4, E, E),
    N ('e', 3, 4, N ('d', 2, 5, E, E), N ('g', 4, 5, E, E))), N ('m', 4, 3, E, E)),
    N ('u', 7, 2, N ('p', 6, 3, E, N ('q', 7, 4, E, E)), E)) then
    Printf.printf "Yes\n"
