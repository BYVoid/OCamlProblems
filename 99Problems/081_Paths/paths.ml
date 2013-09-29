(* Path from one node to another one
Write a function paths g a b that returns all acyclic path p from node a to node b ≠ a in the graph g. The function should return the list of all paths via backtracking. *)

type 'a graph_term = {
  nodes: 'a list;
  edges: ('a * 'a) list
}

let paths graph a target =
  let adjacent node =
    List.fold_left (fun acc edge ->
      if fst edge = node then
        (snd edge) :: acc
      else if snd edge = node then
        (fst edge) :: acc
      else
        acc
    ) [] graph.edges
  in
  let rec find node path =
    Printf.printf "%c\n" node;
    if node = target then
      [List.rev path]
    else
      let adjacent_nodes = adjacent node in
      List.fold_left (fun acc adjacent_node ->
        Printf.printf " ->%c\n" adjacent_node;
        if List.exists ((=) adjacent_node) path then
          acc
        else
          (find adjacent_node (adjacent_node :: path)) @ acc
      ) [] adjacent_nodes
  in
  find a [a]


let graph = {
  nodes = ['b'; 'c'; 'd'; 'f'; 'g'; 'h'; 'k'];
  edges = ['h', 'g';  'k', 'f';  'f', 'b';  'f', 'c';  'c', 'b']
}

let () =
  assert (paths graph 'b' 'k' = [['b'; 'f'; 'k']; ['b'; 'c'; 'f'; 'k']])
