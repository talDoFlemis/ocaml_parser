open Base

let show_expression = Ast.show_expression
let ( let* ) res f = Base.Result.bind res ~f

type precedence =
  [ `Lowest | `Equals | `LessGreater | `Sum | `Product | `Prefix ]
[@@deriving show, ord]

let prec_gte a b = compare_precedence a b >= 0

let token_prec : Token.t -> precedence = function
  | Equal | NotEqual -> `Equals
  | LessThan | GreaterThan -> `LessGreater
  | Plus | Minus -> `Sum
  | Slash | Asterisk -> `Product
  | _ -> `Lowest

type t = {
  toks : Token.t option list;
  current : Token.t option;
  peek : Token.t option;
}
[@@deriving show]

type parse_error = { msg : string; parser : t; statements : Ast.statement list }
[@@deriving show]

let err parser msg statements = Error { parser; msg; statements }

let advance parser =
  match parser.toks with
  | [] -> { parser with current = None; peek = None }
  | gaules :: tl -> { toks = tl; current = parser.peek; peek = gaules }

let advance_until parser f =
  let parser = ref parser in
  while not (f !parser) do
    parser := advance !parser
  done;
  !parser

let chomp_semicolon parser =
  match parser.peek with Some Token.Semicolon -> advance parser | _ -> parser

let next_token parser =
  let parser = advance parser in
  (parser, parser.current)

let expect_peek parser condition =
  match parser.peek with
  | Some tok ->
      if condition tok then Ok (advance parser)
      else Error (Fmt.failwith "missing peeked: %a" pp parser)
  | None -> Error "no peek token"

let peek_is parser token = Option.equal Token.equal parser.peek (Some token)

let expect_assign parser =
  expect_peek parser (function Token.Assign -> true | _ -> false)

let expect_do parser =
  expect_peek parser (function Token.Do -> true | _ -> false)

let expect_begin parser =
  expect_peek parser (function Token.Begin -> true | _ -> false)

let expect_end parser =
  expect_peek parser (function Token.End -> true | _ -> false)

let peek_precedence parser =
  match parser.peek with Some tok -> token_prec tok | _ -> `Lowest

let curr_precedence parser =
  match parser.current with Some tok -> token_prec tok | _ -> `Lowest

let init toks =
  let toks_with_options = List.map toks ~f:(fun tok -> Some tok) in
  let parser = { toks = toks_with_options; current = None; peek = None } in
  let parser = advance parser in
  let parser = advance parser in
  parser

let rec parse parser =
  let rec parse' parser statements =
    match parser.current with
    | Some _ -> (
        match parse_statement parser with
        | Ok (parser, stmt) -> parse' (advance parser) (stmt :: statements)
        | Error msg -> err parser msg statements)
    | None -> Ok (parser, List.rev statements)
  in
  let* _, statements = parse' parser [] in
  Ok (Ast.Program { statements })

and parse_statement parser =
  match parser.current with
  | Some (Token.Ident i) -> parse_assignment parser i
  | Some _ -> parse_expression_statement parser
  | None -> Error "no more tokens"

and parse_assignment parser identifier =
  let* parser = expect_assign parser in
  let parser = advance parser in
  let* parser, value = parse_expression parser `Lowest in
  let parser = chomp_semicolon parser in
  let name = Ast.{ identifier } in
  Ok (parser, Ast.Assignment { name; value })

and parse_expression_statement parser =
  let* parser, expr = parse_expression parser `Lowest in
  let parser = chomp_semicolon parser in
  Ok (parser, Ast.ExpressionStatement expr)

and parse_identifier parser =
  match parser.peek with
  | Some (Ident identifier) -> Ok (advance parser, Ast.{ identifier })
  | _ -> Error "missing ident"

and parse_block parser =
  let parser = advance parser in
  let rec parse_block' parser statements =
    match parser.current with
    | Some Token.End -> Ok (parser, List.rev statements)
    | Some _ ->
        let* parser, statement = parse_statement parser in
        parse_block' (advance parser) (statement :: statements)
    | None -> Error "unexpected eof"
  in
  let* parser, block = parse_block' parser [] in
  Ok (parser, Ast.{ block })

and parse_expression parser prec =
  let* parser, left = parse_prefix_expression parser in
  let rec parse_expression' parser left =
    let peeked = parser.peek |> Option.value ~default:Token.Illegal in
    let prec_peek = token_prec peeked in
    if peek_is parser Token.Semicolon || prec_gte prec prec_peek then
      Ok (parser, left)
    else
      match get_infix_fn parser with
      | Some infix_fn ->
          let parser = advance parser in
          let* parser, left = infix_fn parser left in
          parse_expression' parser left
      | None -> Ok (parser, left)
  in
  parse_expression' parser left

and parse_prefix_expression parser =
  let map_parser = Result.map ~f:(fun v -> (parser, v)) in
  let token = parser.current |> Option.value_exn in
  match token with
  | Token.Ident _ -> expr_parse_identifier parser |> map_parser
  | Token.Integer _ -> expr_parse_number parser |> map_parser
  | Token.String _ -> expr_parse_string parser |> map_parser
  | Token.Minus -> expr_parse_prefix parser token
  | Token.If -> expr_parse_if parser
  | Token.While -> expr_parse_while parser
  | Token.Read -> expr_parse_read parser
  | Token.Write -> expr_parse_write parser
  | tok ->
      Error (Fmt.str "unexpected prefix expr: %a\n %a" Token.pp tok pp parser)

and parse_infix_expression parser left =
  let operator = parser.current |> Option.value_exn in
  let prec = curr_precedence parser in
  let parser = advance parser in
  let* parser, right = parse_expression parser prec in
  Ok (parser, Ast.Infix { left; operator; right })

and get_infix_fn parser =
  let open Token in
  match parser.peek with
  | Some Plus
  | Some Minus
  | Some Slash
  | Some Asterisk
  | Some Equal
  | Some NotEqual
  | Some LessThan
  | Some GreaterThan ->
      Some parse_infix_expression
  | _ -> None

and expr_parse_identifier parser =
  match parser.current with
  | Some (Ident identifier) -> Ok (Ast.Identifier { identifier })
  | _ -> Error "missing number"

and expr_parse_string parser =
  match parser.current with
  | Some (String str) -> Ok (Ast.String str)
  | _ -> Error "missing string"

and expr_parse_number parser =
  match parser.current with
  | Some (Integer num) ->
      let num =
        try Int.of_string num
        with Failure x -> Fmt.failwith "COULD NOT PARSE: '%s' DUE TO %s" num x
      in
      Ok (Ast.Integer num)
  | _ -> Error "missing number"

and expr_parse_prefix parser operator =
  let parser = advance parser in
  let* parser, right = parse_expression parser `Prefix in
  Ok (parser, Ast.Prefix { operator; right })

and expr_parse_if parser =
  let parser = advance parser in
  let* parser, condition = parse_expression parser `Lowest in
  let* parser = expect_begin parser in
  let* parser, consequence = parse_block parser in
  let* parser, alternative =
    match parser.peek with
    | Some Token.Else ->
        let parser = advance parser in
        let* parser = expect_end parser in
        let* parser, block = parse_block parser in
        Ok (parser, Some block)
    | _ -> Ok (parser, None)
  in
  Ok (parser, Ast.If { condition; consequence; alternative })

and expr_parse_while parser =
  let parser = advance parser in
  let* parser, condition = parse_expression parser `Lowest in
  let* parser = expect_do  parser in
  let* parser, body = parse_block parser in
  Ok (parser, Ast.While { condition; body })

and expr_parse_write parser =
  let parser = advance parser in
  let* parser, expression = parse_expression parser `Lowest in
  let parser = chomp_semicolon parser in
  Ok (parser, Ast.Write { expression })

and expr_parse_read parser =
  let* parser, identifier = parse_identifier parser in
  let parser = chomp_semicolon parser in
  Ok (parser, Ast.Read { identifier })

let string_of_statement = function
  | Ast.Assignment stmt ->
      Fmt.str "Assignment: %s = %s"
        (Ast.show_identifier stmt.name)
        (show_expression stmt.value)
  | ExpressionStatement expr -> Fmt.str "EXPR: %s;" (show_expression expr)
  | BlockStatement _ -> assert false

and string_of_ident ident = Ast.(ident.identifier)

let print_node = function
  | Ast.Program program ->
      Fmt.pr "Program: [@.";
      List.iter program.statements ~f:(fun s ->
          Fmt.pr "  %s@." (string_of_statement s));
      Fmt.pr "]@."
  | _ -> failwith "yaya"
