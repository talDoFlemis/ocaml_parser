open Ocaml_parser

let () =
  let tokens =
    [
      Token.If;
      Token.Integer "1";
      Token.Equal;
      Token.Integer "1";
      Token.Begin;
      Token.Write;
      Token.String "Hello, World!";
      Token.Semicolon;
      Token.While;
      Token.Ident "x";
      Token.LessThan;
      Token.Integer "10";
      Token.Do;
      Token.Write;
      Token.Ident "x";
      Token.Semicolon;
      Token.Ident "x";
      Token.Assign;
      Token.Ident "x";
      Token.Plus;
      Token.Integer "1";
      Token.Semicolon;
      Token.Read;
      Token.Ident "x";
      Token.Semicolon;
      Token.End;
      Token.End;
      Token.Semicolon;
    ]
  in

  let parser = Parser.init tokens in
  match Parser.parse parser with
  | Ok ast ->
      print_endline "Parsing successful!";
      Parser.print_node ast
  | Error err ->
      print_endline ("Parsing error: " ^ err.msg);
