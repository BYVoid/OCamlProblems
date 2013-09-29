(* Extract a given number of randomly selected elements from a list.
The selected items shall be returned in a list. We use the Random module but do not initialize it with Random.self_init for reproducibility. *)

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

let () =
  Random.self_init();;
  let result = rand_select [1;2;3;4;5;6;7;8] 4 in
  List.iter (Printf.printf "%i ") result;
  Printf.printf "\n"
