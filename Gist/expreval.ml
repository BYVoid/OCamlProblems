open Core.Std

type token = [
  | `Int of int
  | `LeftParen
  | `RightParen
  | `Plus
  | `Minus
  | `Multiply
  | `Divide
]

type expression = [
  | `Int of int
  | `Plus of expression * expression
  | `Minus of expression * expression
  | `Multiply of expression * expression
  | `Divide of expression * expression
]

let tokenize (expr_str : string) : token list =
  let append_num_token num_buffer tokens =
    let get_num num_buffer =
      match num_buffer with
      | [] -> None
      | _ ->
        let num = List.rev num_buffer |> String.of_char_list |> Int.of_string in
        Some (`Int num)
    in
    match get_num num_buffer with
    | Some token ->
      token :: tokens
    | None ->
      tokens
  in

  let num_buffer, tokens = String.fold expr_str
      ~init:([], [])
      ~f:(fun (num_buffer, tokens) ch ->
          let terminal_token token =
            let tokens = append_num_token num_buffer tokens in
            ([], token :: tokens)
          in
          match ch with
          | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
            (ch :: num_buffer, tokens)
          | '(' -> terminal_token `LeftParen
          | ')' -> terminal_token `RightParen
          | '+' -> terminal_token `Plus
          | '-' -> terminal_token `Minus
          | '*' -> terminal_token `Multiply
          | '/' -> terminal_token `Divide
          | _ -> failwith (sprintf "Invalid char: %c" ch)
        )
  in
  append_num_token num_buffer tokens |> List.rev

let parse_tokens (tokens : token list) : expression =
  let reduce nums opers =
    let reduce_binary cons other_opers =
      match nums with
      | operand2 :: operand1 :: other_nums ->
        let expr = cons (operand1, operand2) in
        (expr :: other_nums, other_opers)
      | _ -> failwith "Number of operands mismatch"
    in
    match opers with
    | [] | `LeftParen :: _ | `RightParen :: _ -> failwith "Paren mismatch"
    | `Plus :: other_opers ->
      reduce_binary (fun arg -> `Plus arg) other_opers
    | `Minus :: other_opers ->
      reduce_binary (fun arg -> `Minus arg) other_opers
    | `Multiply :: other_opers ->
      reduce_binary (fun arg -> `Multiply arg) other_opers
    | `Divide :: other_opers ->
      reduce_binary (fun arg -> `Divide arg) other_opers
  in
  let nums, opers = List.fold tokens
      ~init:([], [])
      ~f:(fun (nums, opers) token ->
          let rank0_oper oper =
            match opers with
            | `LeftParen :: _
            | [] ->
              (nums, oper :: opers)
            | _ ->
              let nums, opers = reduce nums opers in
              (nums, oper :: opers)
          in
          let rank1_oper oper =
            match opers with
            | `LeftParen :: _
            | `Plus :: _
            | `Minus :: _
            | [] ->
              (nums, oper :: opers)
            | _ ->
              let nums, opers = reduce nums opers in
              (nums, oper :: opers)
          in
          match token with
          | `Int num ->
            (`Int num :: nums, opers)
          | `Plus -> rank0_oper `Plus
          | `Minus -> rank0_oper `Minus
          | `Multiply -> rank1_oper `Multiply
          | `Divide -> rank1_oper `Divide
          | `LeftParen ->
            (nums, `LeftParen :: opers)
          | `RightParen ->
            let rec reduce_loop (nums, opers) =
              match opers with
              | `LeftParen :: other_opers -> nums, other_opers
              | [] -> failwith "Paren mismatch"
              | _ ->
                reduce_loop (reduce nums opers)
            in
            reduce_loop (nums, opers)
        )
  in
  let rec reduce_loop (nums, opers) =
    match opers with
    | [] -> nums
    | _ ->
      reduce_loop (reduce nums opers)
  in
  match reduce_loop (nums, opers) with
  | [expr] -> expr
  | _ -> failwith "Error parsing tokens"

let parse (expr_str : string) : expression =
  let tokens = tokenize expr_str in
  parse_tokens tokens
