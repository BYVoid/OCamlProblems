open Core.Std

(* Insert elem to every position in lst and return a list of lists *)
let rec insert_every elem lst : 'a list list =
  match lst with
  | [] -> [[elem]]
  | head :: tail ->
    (elem :: lst) ::
      (* Insert elem to tail and prepend head in the front *)
      (List.map (insert_every elem tail)
        ~f:(fun lst -> head :: lst))

let rec permutations lst : 'a list list =
  match lst with
  | [] -> [[]]
  | head :: tail ->
    List.fold (permutations tail)
      ~init: []
      (* Insert head to every position of every permutation of tail *)
      ~f:(fun acc lst -> (insert_every head lst) @ acc)
