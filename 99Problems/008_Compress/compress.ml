(* Eliminate consecutive duplicates of list elements. *)

(* not tail recursion *)
let rec compress = function
	| a::(b::_ as tail) -> if a = b then compress tail else a::compress tail
	| other -> other

(* tail recursion *)
let compress1 = function
	| [] -> []
	| lst ->
		let rec iter acc = function
			| [] -> acc
			| head::tail -> if (List.hd acc) = head then iter acc tail else iter (head::acc) tail in
		List.rev (iter [List.hd lst] (List.tl lst))

let () =
	let result = compress1 [1;1;1;1;2;3;3;1;1;4;5;5;5;5] in
  List.iter (Printf.printf "%i ") result;
  Printf.printf "\n"
