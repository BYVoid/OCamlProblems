(* Generate a random permutation of the elements of a list. *)

let rand_select lst count =
  let extract pos lst =
    let rec iter count acc = function
      | elem :: tail ->
        if count < pos then
          iter (count + 1) (elem::acc) tail
        else
          elem, acc @ tail
      | [] -> raise Not_found in
    iter 0 [] lst in
  let len = List.length lst in
  let rec iter current acc lst =
    if current < count then
      let pos = Random.int (len - current) in
      let elem, remains = extract pos lst in
      iter (current + 1) (elem::acc) remains
    else
      acc
    in
  iter 0 [] lst

let permutation lst =
  rand_select lst (List.length lst)

let () =
  Random.self_init();;
  let result = permutation [1;2;3;4;5;6;7;8;9] in
  List.iter (Printf.printf "%i ") result;
  Printf.printf "\n"
