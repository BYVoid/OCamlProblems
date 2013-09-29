(* 
Layout a binary tree (2)

An alternative layout method is depicted in the above illustration. Find out the rules and write the corresponding OCaml function.

Hint: On a given level, the horizontal distance between neighboring nodes is constant.
*)

type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree

type 'a pos_binary_tree =
  | E (* represents the empty tree *)
  | N of 'a * int * int * 'a pos_binary_tree * 'a pos_binary_tree

let rec pow num p =
  if p <= 0 then
    1
  else
    let sub = pow num (p / 2) in
    if p mod 2 = 0 then
      sub * sub
    else
      num * sub * sub

let rec depth = function
  | Empty -> 0
  | Node (_, left, right) -> max (depth left) (depth right) + 1

let layout_binary_tree tree =
  let height = depth tree in
  let rec layout_binary_tree' depth x = function
    | Empty -> E
    | Node (elem, left, right) ->
      let width = pow 2 (height - depth - 1) in
      let new_left = layout_binary_tree' (depth + 1) (x - width) left in
      let new_right = layout_binary_tree' (depth + 1) (x + width) right in
      N (elem, x, depth, new_left, new_right)
  in
  layout_binary_tree' 1 (pow 2 (height - 1) - 1) tree

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
  if layout_binary_tree tree = N ('n', 15, 1, N ('k', 7, 2, N ('c', 3, 3, N ('a', 1, 4, E, E), N ('e', 5, 4, N ('d', 4, 5, E, E), N ('g', 6, 5, E, E))), N ('m', 11, 3, E, E)), N ('u', 23, 2, N ('p', 19, 3, E, N ('q', 21, 4, E, E)), E))  then
    Printf.printf "Yes\n"
