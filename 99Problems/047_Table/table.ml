(* Truth tables for logical expressions (2 variables).
Define a function, table2 which returns the truth table of a given logical expression in two variables (specified as arguments). The return value must be a list of triples containing (value_of_a, balue_of_b, value_of_expr). *)

type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr

let rec evaluate vars = function
  | Var var -> List.assoc var vars
  | Not exp -> not (evaluate vars exp)
  | And (exp1, exp2) -> (evaluate vars exp1) && (evaluate vars exp2)
  | Or (exp1, exp2) -> (evaluate vars exp1) || (evaluate vars exp2)

let table vars exp =
  let rec iter binded acc = function
    | var :: tail ->
      let results = iter ((var, false) :: binded) acc tail in
      iter ((var, true) :: binded) results tail
    | [] -> (List.rev binded, evaluate binded exp) :: acc
  in
  iter [] [] vars

let () =
  if table ["a"; "b"] (And(Var "a", Or(Var "a", Var "b")))
    = [["a", true; "b", true], true;
       ["a", true; "b", false], true;
       ["a", false; "b", true], false;
       ["a", false; "b", false], false ] then
    Printf.printf "Yes\n";
