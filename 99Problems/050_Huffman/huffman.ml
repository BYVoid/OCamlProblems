(* Huffman code.
First of all, consult a good book on discrete mathematics or algorithms for a detailed description of Huffman codes (you can start with the Wikipedia page)!

We suppose a set of symbols with their frequencies, given as a list of Fr(S,F) terms. Example: fs = [Fr(a,45); Fr(b,13); Fr(c,12); Fr(d,16); Fr(e,9); Fr(f,5)]. Our objective is to construct a list Hc(S,C) terms, where C is the Huffman code word for the symbol S. In our example, the result could be hs = [Hc(a,'0'); Hc(b,'101'); Hc(c,'100'); Hc(d,'111'); Hc(e,'1101'); hc(f,'1100')] [hc(a,'01'),...etc.]. The task shall be performed by the function huffman defined as follows: huffman(fs) returns the Huffman code table for the frequency table fs *)

type frequency = Fr of char * int
type huffman_code = Hc of char * string

type huffman_tree =
  | Leaf of char * int
  | Node of huffman_tree * huffman_tree * int

(* Get the frequence of the node *)
let freq (node: huffman_tree) =
  match node with
    | Leaf (_, freq) | Node (_, _, freq) -> freq

(* Sort the list of nodes by frequency and return the minimal two nodes with the others*)
let min_two (nodes: huffman_tree list) =
  let sorted = List.sort (fun a b -> (freq a) - (freq b)) nodes in
  match sorted with
    | a :: b :: tail -> a, b, tail
    | _ -> raise Not_found

let huffman fs =
  let tree = List.map (fun (Fr (ch, freq)) -> Leaf (ch, freq)) fs in
  let tree_length = List.length tree in
  let rec cons i tree =
    if i < tree_length then
      (* Find the minimal two nodes and merge them *)
      let node1, node2, others = min_two tree in
      let new_node = Node (node1, node2, ((freq node1) + (freq node2))) in
      cons (i + 1) (new_node :: others)
    else
      List.hd tree
  in
  let huffman_tree = cons 1 tree in
  (* Allocate code for every node *)
  let rec code node prefix (acc: huffman_code list) =
    match node with
      | Node (left, right, _) ->
        let acc = code left (prefix ^ "0") acc in
        code right (prefix ^ "1") acc
      | Leaf (ch, _) ->
        (Hc (ch, prefix)) :: acc
  in
  code huffman_tree "" []

let () =
  let fs = [Fr('a',45); Fr('b',13); Fr('c',12); Fr('d',16); Fr('e',9); Fr('f',5)] in
  let hs = huffman fs in
  if hs = [Hc ('d', "111"); Hc ('e', "1101"); Hc ('f', "1100"); Hc ('b', "101"); Hc ('c', "100"); Hc ('a', "0")] then
  Printf.printf "Yes\n";
