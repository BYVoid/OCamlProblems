(*  Cycle from a given node
Write a functions cycle g a that returns a closed path (cycle) p starting at a given node a in the graph g. The predicate should return the list of all cycles via backtracking. *)

type 'a graph_term = {
  nodes: 'a list;
  edges: ('a * 'a) list
}

let adjacent graph node =
  List.fold_left (fun acc edge ->
    if fst edge = node then
      (snd edge) :: acc
    else if snd edge = node then
      (fst edge) :: acc
    else
      acc
  ) [] graph.edges

let paths graph a target =
  let rec find node path =
    Printf.printf "%c\n" node;
    if node = target then
      [List.rev path]
    else
      let adjacent_nodes = adjacent graph node in
      List.fold_left (fun acc adjacent_node ->
        Printf.printf " ->%c\n" adjacent_node;
        if List.exists ((=) adjacent_node) path then
          acc
        else
          (find adjacent_node (adjacent_node :: path)) @ acc
      ) [] adjacent_nodes
  in
  find a [a]

let cycles graph a =
  let adjacent_nodes = adjacent graph a in
  List.fold_left (fun acc node ->
    List.map (fun path -> a :: path) (paths graph node a) @ acc
  ) [] adjacent_nodes


let graph = {
  nodes = ['b'; 'c'; 'd'; 'f'; 'g'; 'h'; 'k'];
  edges = ['h', 'g';  'k', 'f';  'f', 'b';  'f', 'c';  'c', 'b']
}

let () =
  assert (cycles graph 'f' = [['f'; 'k'; 'f']; ['f'; 'b'; 'f']; ['f'; 'b'; 'c'; 'f']; ['f'; 'c'; 'f']; ['f'; 'c'; 'b'; 'f']]
)
