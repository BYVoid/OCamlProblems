(* Modify the result of the previous problem in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.

Since OCaml lists are homogeneous, one needs to define a type to hold both single elements and sub-lists. *)

type 'a rle =
  | One of 'a
  | Many of (int * 'a);;

let encode lst =
	let append number elem lst =
		if number = 1 then
			One elem::lst
		else
			Many (number, elem)::lst in
	let rec iter current acc = function
		| a::(b::_ as tail) ->
			if a = b then
				iter (current + 1) acc tail
			else
				iter 0 (append (current + 1) a acc) tail
		| [a] -> append (current + 1) a acc
		| [] -> [] in
	List.rev (iter 0 [] lst)

let () =
	let result = encode [`a;`a;`a;`a;`b;`c;`c;`a;`a;`d;`e;`e;`e;`e]
  = [Many (4,`a) ; One `b ; Many (2,`c) ; Many (2,`a) ; One `d ; Many (4,`e)] in
	if result then
		Printf.printf "Yes\n"
	