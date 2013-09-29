(* Construct the minimal spanning tree
Write a function ms_tree graph to construct the minimal spanning tree of a
given labelled graph. A labelled graph will be represented as follows:
*)

type ('a, 'b) labeled_graph = {
  nodes : 'a list;
  edges : ('a * 'a * 'b) list
}

let ms_tree graph =
  let num_nodes = List.length graph.nodes in
  let add_to nodes new_node =
    if List.exists (fun node -> node = new_node) nodes then
      nodes
    else
      new_node :: nodes
  in
  let get_reached_edges tree =
    List.filter (fun edge ->
      let a, b, _ = edge in
      not (List.exists (fun tree_edge -> tree_edge = edge ) tree.edges)
      && (List.exists (fun node -> node = a || node = b) tree.nodes)
    ) graph.edges
  in
  let rec prim tree =
    if List.length tree.nodes = num_nodes then
      tree
    else
      let edges_reached = List.sort (fun (_, _, a) (_, _, b) -> a - b)
          (get_reached_edges tree) in
      let span_edge = List.hd edges_reached in
      let a, b, _ = span_edge in
      let nodes = add_to tree.nodes a in
      let nodes = add_to nodes b in
      prim {nodes; edges = span_edge :: tree.edges}
  in
  match graph.nodes with
  | init :: _ ->
    prim {nodes = [init]; edges = []}
  | _ -> {nodes = graph.nodes; edges = []}

let graph =  {
  nodes = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'];
  edges = [('a', 'b', 5); ('a', 'd', 3); ('b', 'c', 2); ('b', 'e', 4);
           ('c', 'e', 6); ('d', 'e', 7); ('d', 'f', 4); ('d', 'g', 3);
           ('e', 'h', 5); ('f', 'g', 4); ('g', 'h', 1)]
}

let () =
  assert (ms_tree graph = {
    nodes = ['e'; 'c'; 'b'; 'f'; 'h'; 'g'; 'd'; 'a'];
    edges = [('b', 'e', 4); ('b', 'c', 2); ('a', 'b', 5); ('f', 'g', 4);
             ('d', 'f', 4); ('g', 'h', 1); ('d', 'g', 3); ('a', 'd', 3)]
  })
