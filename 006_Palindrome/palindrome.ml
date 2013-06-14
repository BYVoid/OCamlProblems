(* Find out whether a list is a palindrome. *)

let is_palindrome lst =
  lst = List.rev lst

let () =
  if is_palindrome [1;2;3;2;1] then
    Printf.printf "Yes\n"
  else
    Printf.printf "No\n"
