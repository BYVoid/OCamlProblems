(* Extract a slice from a list.
Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included). Start counting the elements with 0 (this is the way the List module numbers elements). *)

let slice lst start_pos end_pos =
	let rec iter skip count acc = function
		| elem::tail ->
			if skip < start_pos then
				iter (skip + 1) count acc tail
			else if (skip + count) < end_pos then
				iter skip (count + 1) (elem::acc) tail
			else
				elem::acc
		| [] -> acc in
	List.rev (iter 0 0 [] lst)

let () =
	let result = slice [`a;`b;`c;`d;`e;`f;`g;`h;`i;`j] 2 6 = [`c;`d;`e;`f;`g] in
	if result then
		Printf.printf "Yes\n"
