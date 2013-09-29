(* Sorting a list of lists according to length of sublists
a) We suppose that a list contains elements that are lists themselves. The objective is to sort the elements of this list according to their length. E.g. short lists first, longer lists later, or vice versa.

b) Again, we suppose that a list contains elements that are lists themselves. But this time the objective is to sort the elements of this list according to their length frequency; i.e., in the default, where sorting is done ascendingly, lists with rare lengths are placed first, others with a more frequent length come later. *)

let length_sort lst =
  List.sort (fun a b ->
    (List.length a) - (List.length b)
  ) lst

let frequency_sort lst =
  let lengths = List.map List.length lst in
  let ass = List.map2 (fun elem cur_len ->
    let freq = List.fold_left (fun sum len ->
      if len = cur_len then sum + 1 else sum
    ) 0 lengths in
    elem, freq
  ) lst lengths in
  let sorted = List.sort (fun a b ->
    (snd a) - (snd b)
  ) ass in
  List.map fst sorted

let () =
  let result = (length_sort [ [`a;`b;`c]; [`d;`e]; [`f;`g;`h]; [`d;`e];
                [`i;`j;`k;`l]; [`m;`n]; [`o] ]
  = [[`o]; [`d; `e]; [`d; `e]; [`m; `n]; [`a; `b; `c]; [`f; `g; `h];
     [`i; `j; `k; `l]]) && (frequency_sort [ [`a;`b;`c]; [`d;`e]; [`f;`g;`h];
       [`d;`e]; [`i;`j;`k;`l]; [`m;`n]; [`o] ] = [[`i; `j; `k; `l];
       [`o]; [`a; `b; `c]; [`f; `g; `h]; [`d; `e]; [`d; `e];
     [`m; `n]]) in
  if result then
    Printf.printf "Yes\n"
