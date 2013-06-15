(* Pack consecutive duplicates of list elements into sublists. *)

let pack lst =
	let rec iter current acc = function
		| a::(b::_ as tail) ->
			if a = b then
				iter (a::current) acc tail
			else
				iter [] ((a::current)::acc) tail
		| [a] -> (a::current)::acc
		| [] -> [] in
	List.rev (iter [] [] lst)

let () =
	let result = pack [`a;`a;`a;`a;`b;`c;`c;`a;`a;`d;`d;`e;`e;`e;`e]
	  = [[`a;`a;`a;`a]; [`b]; [`c;`c]; [`a;`a]; [`d;`d]; [`e;`e;`e;`e]] in
	if result then
		Printf.printf "Yes\n"
	