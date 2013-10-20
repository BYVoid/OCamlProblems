open Core.Std

let memoize f =
  let table = Hashtbl.Poly.create () in
  fun x ->
    match Hashtbl.find table x with
    | Some y -> y
    | None ->
      let y = f x in
      Hashtbl.add_exn table ~key:x ~data:y;
      y

let fib_norec fib i =
  if i <= 1 then
    i
  else
    fib (i - 1) + fib (i - 2)

let make_rec f_norec =
  let rec f x = f_norec f x in
  f

let memo_rec f_norec x =
  let fref = ref (fun _ -> assert false) in
  let f = memoize (fun x -> f_norec !fref x) in
  fref := f;
  f x

let lazy_memo_rec f_norec x =
  let rec f = lazy (memoize (fun x ->
      f_norec (Lazy.force f) x))
  in
  (Lazy.force f) x

let fib = memo_rec fib_norec
let fib2 = lazy_memo_rec fib_norec

let time f =
  let start = Time.now () in
  let x = f () in
  let stop = Time.now () in
  printf "Time: %s\n" (Time.Span.to_string (Time.diff stop start));
  x

let () =
  ignore (time (fun () -> fib 40));
  ignore (time (fun () -> fib2 40))
