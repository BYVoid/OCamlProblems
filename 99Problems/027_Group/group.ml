(* Group the elements of a set into disjoint subsets.
a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? Write a function that generates all the possibilities and returns them in a list.

b) Generalize the above function in a way that we can specify a list of group sizes and the function will return a list of groups. *)

let combination count lst =
  let rec comb num current unselected lst =
    if num < count then
      match lst with
      | elem :: tail ->
        let result = comb (num + 1) (elem :: current) unselected tail in
        result @ (comb num current (elem :: unselected) tail)
      | [] -> []
    else
      [current, unselected @ lst]
    in
  comb 0 [] [] lst

let group lst groups =
  let rec comb (groups: int list) lst = match groups with
    | count :: groups_tail ->
      let selected = combination count lst in
      let result = List.map (fun (sel, remains) ->
        let res = comb groups_tail remains in
        if res = [] then
          [[sel]]
        else
          List.map (fun group ->
            sel :: group
          ) res
      ) selected in
      List.flatten result
    | [] -> []
    in
  comb groups lst

let () =
  let result = group [1;2;3;4] [2; 1] = [[[2; 1]; [3]]; [[2; 1]; [4]]; [[3; 1]; [2]]; [[3; 1]; [4]]; [[4; 1]; [3]]; [[4; 1]; [2]]; [[3; 2]; [1]]; [[3; 2]; [4]]; [[4; 2]; [3]]; [[4; 2]; [1]]; [[4; 3]; [2]]; [[4; 3]; [1]]] in
  if result then
    Printf.printf "Yes\n"
