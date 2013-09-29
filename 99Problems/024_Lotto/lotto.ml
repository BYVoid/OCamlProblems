(* Lotto: Draw N different random numbers from the set 1..M.
The selected numbers shall be returned in a list. *)

let range first last =
  let rec iter current last acc =
    if current < last then
      iter (current + 1) last (current :: acc)
    else
      current :: acc
    in
  if first < last then
    List.rev (iter first last [])
  else
    iter last first []

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

let lotto_select n m =
  rand_select (range 1 m) n;;

let () =
  Random.self_init();;
  let result = lotto_select 6 49 in
  List.iter (Printf.printf "%i ") result;
  Printf.printf "\n"
