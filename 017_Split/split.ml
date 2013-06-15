(* Split a list into two parts; the length of the first part is given. *)

let split lst position =
	let rec iter count acc = function
		| elem::tail ->
			if count < position then
				iter (count + 1) (elem::acc) tail
			else
				(List.rev (elem::acc), tail)
		| [] -> (List.rev acc, []) in
	iter 1 [] lst

let () =
	let result = (split [`a;`b;`c;`d;`e;`f;`g;`h;`i;`j] 3
  	= ([`a;`b;`c] , [`d;`e;`f;`g;`h;`i;`j])) && 
		(split [`a;`b;`c;`d] 5 = ([`a; `b; `c; `d], [])) in
	if result then
		Printf.printf "Yes\n"
