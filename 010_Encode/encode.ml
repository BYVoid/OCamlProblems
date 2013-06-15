(* Pack consecutive duplicates of list elements into sublists. *)

let encode lst =
	let rec iter current acc = function
		| a::(b::_ as tail) ->
			if a = b then
				iter (current + 1) acc tail
			else
				iter 0 ((current + 1, a)::acc) tail
		| [a] -> (current + 1, a)::acc
		| [] -> [] in
	List.rev (iter 0 [] lst)

let () =
	let result = encode [`a;`a;`a;`a;`b;`c;`c;`a;`a;`d;`e;`e;`e;`e]
  = [4,`a ; 1,`b ; 2,`c ; 2,`a ; 1,`d ; 4,`e] in
	if result then
		Printf.printf "Yes\n"
	