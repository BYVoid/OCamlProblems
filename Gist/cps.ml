let rec fib num =
  match num with
  | 0 -> 1
  | 1 -> 1
  | _ -> fib (num - 1) + fib (num - 2)

let rec fib_cps num cont =
  match num with
  | 0 -> cont 1
  | 1 -> cont 1
  | _ -> fib_cps (num - 1) (fun res1 ->
    fib_cps (num - 2) (fun res2 ->
      cont (res1 + res2)
    )
  )

let () =
  Printf.printf "%d\n" (fib 5);
  fib_cps 5 (fun res ->
    Printf.printf "%d\n" res
  );
  Printf.printf "%d\n" (fib_cps 5 (fun res -> res))
