(* Count the nodes of a multiway tree *)

type 'a mult_tree = T of 'a * 'a mult_tree list;;

let rec count_nodes (T (_, subtree)) =
  let rec count_woods = function
    | [] -> 0
    | tree :: tail -> (count_nodes tree) + (count_woods tail)
  in
  1 + count_woods subtree

let () =
  Printf.printf "%d\n" (count_nodes (T('a', [T('f',[]) ])));
  Printf.printf "%d\n" (count_nodes
    (T('a', [T('f',[T('g',[])]); T('c',[]); T('b',[T('d',[]); T('e',[])])]) ));
