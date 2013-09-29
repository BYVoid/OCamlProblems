(* Write a function s_tree g to construct (by backtracking) all spanning trees
of a given graph g. With this predicate, find out how many spanning trees
there are for the graph depicted to the left. The data of this example graph
can be found in the test below. When you have a correct solution for the s_tree
function, use it to define two other useful functions: is_tree graph and
is_connected Graph. Both are five-minutes tasks!
*)

type 'a graph_term = {
  nodes: 'a list;
  edges: ('a * 'a) list
}

let get_reached_nodes edges =
  let add_to nodes new_node =
    if List.exists (fun node -> node = new_node) nodes then
      nodes
    else
      new_node :: nodes
  in
  List.fold_left (fun nodes edge ->
    let nodes = add_to nodes (fst edge) in
    add_to nodes (snd edge)
  ) [] edges

let s_tree graph =
  let num_nodes = List.length graph.nodes in
  let rec select edges selected =
    let continue =
      match edges with
      | [] -> []
      | edge :: rest ->
        let sel = select rest (edge :: selected) in
        let unsel = select rest selected in
        sel @ unsel
    in
    if List.length selected = num_nodes - 1 then
      let nodes_reached = get_reached_nodes selected in
      if List.length nodes_reached = num_nodes then
        [selected]
      else
        continue
    else
      continue
  in
  select graph.edges []

let graph = {
  nodes = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'];
  edges = [('a', 'b'); ('a', 'd'); ('b', 'c'); ('b', 'e');
    ('c', 'e'); ('d', 'e'); ('d', 'f'); ('d', 'g');
    ('e', 'h'); ('f', 'g'); ('g', 'h')] 
};;

let () =
  assert (List.length (s_tree graph) = 178)
