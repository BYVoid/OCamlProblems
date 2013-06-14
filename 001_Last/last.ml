#use "topfind"
#thread
#require "core.top"

open Core.Std

let rec last1 = function
  | [] -> None
  | [x] ->  Some x
  | _::tail -> last1 tail

let last2 lst =
  List.fold_left lst ~init:None ~f:(fun acc x -> Some x)

let last3 lst =
  List.reduce lst ~f:(fun acc x -> x)

let last4 lst =
  List.fold_right lst ~init:None ~f:(fun x acc -> match acc with
    | None -> Some x
    | Some x as last -> last
  )

let last5 lst = match List.rev lst with
  | [] -> None
  | head::tail -> Some head

let () =
  let result = last1 [1;2;3] in
  match result with
    | None -> Printf.printf "Empty list\n"
    | Some x -> Printf.printf "%i\n" x
