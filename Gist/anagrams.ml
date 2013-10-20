(* http://www.careercup.com/question?id=15022713 *)

open Core.Std

let group_anagrams anagrams =
  let map = List.fold anagrams
      ~init:(Map.empty String.comparator)
      ~f:(fun map word ->
          let word_sorted =
            (String.to_list word)
            |> (List.sort ~cmp:Char.compare)
            |> String.of_char_list
          in
          let words = match Map.find map word_sorted with
            | None -> [word]
            | Some words -> word :: words
          in
          Map.add map ~key:word_sorted ~data:words
        )
  in
  Map.fold map ~init:[] ~f:(fun ~key ~data acc -> data :: acc)

let () =
  assert (
    (group_anagrams ["cat"; "act"; "bat"; "tab"; "b"])
    = [["b"]; ["act"; "cat"]; ["tab"; "bat"]]
  )
