(* Graph isomorphism

Two graphs G1(N1,E1) and G2(N2,E2) are isomorphic if there is a bijection f:
N1 -> N2 such that for any nodes X,Y of N1, X and Y are adjacent if and only
if f(X) and f(Y) are adjacent.

Write a predicate that determines whether two graphs are isomorphic.
Hint: Use an open-ended list to represent the function f.
*)

type 'a graph_term = {
  nodes: 'a list;
  edges: ('a * 'a) list
}

let can_map_to graph1 graph2 map =
  let transform node :'a =
    let node = List.fold_left2 (fun current x y ->
      match current with
      | None ->
        if x = node then
          Some y
        else
          None
      | Some _ -> current
    ) None graph1.nodes map in
    match node with
    | Some node -> node
    | None -> assert false
  in
  let transformed :('a * 'a) list = List.map (fun edge ->
    let a, b = edge in
    let transformed_edge : ('a * 'a) = (transform a, transform b) in
    transformed_edge
  ) graph1.edges in
  let cmp (a1, b1) (a2, b2) =
    if a1 < a2 then
      -1
    else if a1 > a2 then
      1
    else if b1 < b2 then
      -1
    else
      1
  in
  (List.sort cmp transformed) = (List.sort cmp graph2.edges)

let isomorphic graph1 graph2 =
  let nodes = graph2.nodes in
  let rec permute map :bool =
    if List.length map = List.length nodes then
      can_map_to graph1 graph2 map
    else
      List.fold_left (fun is node ->
        if is = true then
          true
        else
          if List.exists (fun n -> n = node) map then
            is
          else
            permute (node :: map)
      ) false graph1.nodes
  in
  permute []

let graph1 = {
  nodes = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'];
  edges = [('a', 'b'); ('a', 'd'); ('b', 'c'); ('b', 'e');
    ('c', 'e'); ('d', 'e'); ('d', 'f'); ('d', 'g');
    ('e', 'h'); ('f', 'g'); ('g', 'h')] 
}

let graph2 = {
  nodes = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'];
  edges = [('h', 'b'); ('h', 'd'); ('b', 'c'); ('b', 'e');
    ('c', 'e'); ('d', 'e'); ('d', 'f'); ('d', 'g');
    ('e', 'a'); ('f', 'g'); ('g', 'a')] 
}

let graph3 = {
  nodes = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'];
  edges = [('h', 'b'); ('h', 'd'); ('b', 'c'); ('b', 'e');
    ('c', 'e'); ('d', 'e'); ('d', 'f'); ('d', 'g');
    ('e', 'a'); ('f', 'g'); ('g', 'b')] 
}

let () =
  assert (isomorphic graph1 graph2 = true);
  assert (isomorphic graph1 graph1 = true);
  assert (isomorphic graph2 graph1 = true);
  assert (isomorphic graph1 graph3 = false);
  assert (isomorphic graph2 graph3 = false);
