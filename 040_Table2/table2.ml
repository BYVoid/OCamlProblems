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

let table2 var1 var2 exp =
  [(true, true, evaluate [(var1, true); (var2, true)] exp);
  (true, false, evaluate [(var1, true); (var2, false)] exp);
  (false, true, evaluate [(var1, false); (var2, true)] exp);
  (false, false, evaluate [(var1, false); (var2, false)] exp)]

let () =
  if table2 "a" "b" (And(Var "a", Or(Var "a", Var "b")))
  = [(true, true, true);
     (true, false, true);
     (false, true, false);
     (false, false, false) ] then
    Printf.printf "Yes\n";
