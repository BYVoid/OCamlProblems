type 'a stream =
  | Nil
  | Cons of 'a * 'a stream lazy_t

(* the natural numbers *)
let rec from (n : int) : int stream =
  Cons (n, lazy (from (n + 1)))

let naturals = from 0

(* head of a stream *)
let hd (llst : 'a stream) : 'a =
  match llst with
  | Nil -> failwith "hd"
  | Cons (head, _) -> head

(* tail of a stream *)
let tl (llst : 'a stream) : 'a stream =
  match llst with
  | Nil -> failwith "tl"
  | Cons (_, tail) -> Lazy.force tail

(* n-th element of a stream *)
let rec nth (llst : 'a stream) (n : int) : 'a =
  if n = 0 then
    hd llst
  else
    nth (tl llst) (n - 1)

(* make a stream from a list *)
let of_list (lst : 'a list) : 'a stream =
  List.fold_left (fun acc head ->
      Cons (head, lazy acc)
    ) Nil (List.rev lst)

(* make a list from the first n elements of a stream *)
let take (llst : 'a stream) (n : int) : 'a list =
  let rec take_impl llst n acc =
    if n <= 0 then
      acc
    else
      match llst with
      | Nil -> acc
      | _ -> take_impl (tl llst) (n - 1) (hd llst :: acc)
  in
  List.rev (take_impl llst n [])

let fold_left (llst : 'a stream) ~(init : 'b) ~(f : 'b -> 'a -> 'b) : 'b =
  let rec fold_left_impl (llst : 'a stream) (acc : 'b) =
    match llst with
    | Nil -> acc
    | Cons (head, tail) ->
      fold_left_impl (Lazy.force tail) (f acc head)
  in
  fold_left_impl llst init

let rec filter (f : 'a -> bool) (s : 'a stream) : 'a stream =
  match s with
  | Nil -> Nil
  | Cons (x, g) ->
    if f x then
      Cons (x, lazy (filter f (Lazy.force g)))
    else
      filter f (Lazy.force g)

let rec map2 (f: 'a -> 'b -> 'c)
    (s : 'a stream) (t : 'b stream) : 'c stream =
  match (s, t) with
  | (Nil, Nil) -> Nil
  | (Cons (x, g), Cons (y, h)) ->
    Cons (f x y, lazy (map2 f (Lazy.force g) (Lazy.force h)))
  | _ -> failwith "map2"

let () =
  let llst : int stream = of_list [1;5;6;8;3;2] in
  let lst : int list = take llst 5 in
  List.iter (fun elem -> Printf.printf "%d " elem) lst

(*
https://github.com/ocaml-batteries-team/batteries-included/blob/master/src/batLazyList.ml
http://www.cs.cornell.edu/courses/cs3110/2011sp/lectures/lec24-streams/streams.htm
*)
