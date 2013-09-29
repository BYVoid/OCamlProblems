(* There is no nested list type in OCaml, so we need to define one
     first. A node of a nested list is either an element, or a list of
     nodes. *)

type 'a node =
  | One of 'a
  | Many of 'a node list

let rec flatten = function
	| [] -> []
	| head::tail -> match head with
		| One x -> x::(flatten tail)
		| Many x -> (flatten x) @ (flatten tail)

let () =
	let result = flatten [ One 1 ; Many [ One 2 ; Many [ One 3 ; One 4 ] ; One 5 ] ] in
  List.iter (Printf.printf "%i ") result;
  Printf.printf "\n"
