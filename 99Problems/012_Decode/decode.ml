(* Given a run-length code list generated as specified in the previous problem, construct its uncompressed version. *)

type 'a rle =
  | One of 'a
  | Many of (int * 'a);;

let decode lst =
	let rec iter acc = function
		| One elem :: tail | Many (1, elem) :: tail -> iter (elem::acc) tail
		| Many (count, elem) :: tail -> iter (elem::acc) (Many (count - 1, elem) :: tail)
		| [] -> acc in
	List.rev (iter [] lst)

let () =
	let result = decode [Many (4,`a); One `b; Many (2,`c); Many (2,`a); One `d; Many (4,`e)]
  	= [`a;`a;`a;`a;`b;`c;`c;`a;`a;`d;`e;`e;`e;`e] in
	if result then
		Printf.printf "Yes\n"
