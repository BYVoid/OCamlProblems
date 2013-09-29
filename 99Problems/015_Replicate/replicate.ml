(* Replicate the elements of a list a given number of times. *)

let replicate lst times =
	let rec iter count acc = function
		| elem::tail as lst ->
			if count < times then
				iter (count + 1) (elem::acc) lst
			else
				iter 1 (elem::acc) tail
		| [] -> acc in
	List.rev (iter 1 [] lst)

let () =
	let result = replicate [`a;`b;`c] 3 = [`a;`a;`a;`b;`b;`b;`c;`c;`c] in
	if result then
		Printf.printf "Yes\n"
