(* Conversions
Write functions to convert between the different graph representations. With these functions, all representations are equivalent; i.e. for the following problems you can always pick freely the most convenient form. This problem is not particularly difficult, but it's a lot of work to deal with all the special cases. *)

type 'a graph_term = {
  nodes: 'a list;
  edges: ('a * 'a) list
}

type 'a adjacent_list = ('a * ('a list)) list

let graph_to_adjacent (graph: 'a graph_term) : 'a adjacent_list=
  let rec gen acc = function
    | [] -> acc
    | node :: tail ->
      let adjacent_edges = List.filter (fun (a, b) ->
        node = a || node = b
      ) graph.edges in
      let adjacent_nodes = List.map (fun (a, b) ->
        if node = a then b else a
      ) adjacent_edges in
      let item = (node, adjacent_nodes) in
      gen (item :: acc) tail
  in
  gen [] graph.nodes

let adjacent_to_graph (adjacents: 'a adjacent_list) =
  let add_to new_elem lst =
    let existing = List.exists (fun elem ->
      elem = new_elem || ((fst elem) = (snd new_elem) && (snd elem) = (fst new_elem))
    ) lst in
    if existing then
      lst
    else
      new_elem :: lst
  in
  let rec gen nodes edges = function
    | [] -> {nodes; edges}
    | (elem, adjacent) :: tail ->
      let edges = List.fold_left (fun edges edge -> add_to (elem, edge) edges) edges adjacent in
      gen (elem :: nodes) edges tail
  in
  gen [] [] adjacents


let graph = {
  nodes = ['b'; 'c'; 'd'; 'f'; 'g'; 'h'; 'k'];
  edges = ['h', 'g';  'k', 'f';  'f', 'b';  'f', 'c';  'c', 'b']
}

let () =
  let adjacent = graph_to_adjacent graph in
  assert (adjacent = [('k', ['f']); ('h', ['g']); ('g', ['h']); ('f', ['k'; 'b'; 'c']); ('d', []); ('c', ['f'; 'b']); ('b', ['f'; 'c'])]);
  let reconstruction = adjacent_to_graph adjacent in
  let graph = {nodes = ['b'; 'c'; 'd'; 'f'; 'g'; 'h'; 'k']; edges = [('c', 'b'); ('f', 'c'); ('f', 'b'); ('h', 'g'); ('k', 'f')]} in
  assert (reconstruction = graph)
