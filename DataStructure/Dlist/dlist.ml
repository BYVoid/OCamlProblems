(* See http://hackage.haskell.org/package/dlist-0.5/src/Data/DList.hs *)

type t = (int list -> int list)

let to_list dlist =
  dlist []

let from_list lst : t =
  (fun tail ->
    lst @ tail
  )

let concat dlist1 dlist2 =
  (fun tail ->
    dlist1 (dlist2 tail)
  )
