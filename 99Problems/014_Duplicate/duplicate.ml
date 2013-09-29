(* Duplicate the elements of a list. *)

let duplicate lst =
	let rec iter acc = function
		| elem::tail -> iter (elem::elem::acc) tail
		| [] -> acc in
	List.rev (iter [] lst)

let () =
	let result = duplicate [`a;`b;`c;`c;`d] = [`a;`a;`b;`b;`c;`c;`c;`c;`d;`d] in
	if result then
		Printf.printf "Yes\n"
