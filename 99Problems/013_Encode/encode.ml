(* Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem “Pack consecutive duplicates of list elements into sublists”, but only count them. As in problem “Modified run-length encoding”, simplify the result list by replacing the singleton lists (1 X) by X. *)

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
	