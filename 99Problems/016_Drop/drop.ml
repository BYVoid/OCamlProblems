(* Drop every N'th element from a list. *)

let drop lst cycle =
	let rec iter count acc = function
		| elem::tail ->
			if count < cycle then
				iter (count + 1) (elem::acc) tail
			else
				iter 1 acc tail
		| [] -> acc in
	List.rev (iter 1 [] lst)

let () =
	let result = drop [`a;`b;`c;`d;`e;`f;`g;`h;`i;`j] 3 = [`a;`b;`d;`e;`g;`h;`j] in
	if result then
		Printf.printf "Yes\n"
