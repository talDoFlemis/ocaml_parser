type t =
  | Illegal
  (* Items *)
  | Ident of string
  | Integer of string
  | String of string
  (* Operators *)
  | Assign
  | Plus
  | Minus
  | Asterisk
  | Slash
  | LessThan
  | GreaterThan
  | Equal
  | NotEqual
  (* Delimiters *)
  | Semicolon
  (* Keyword *)
  | If
  | Else
  | While
  | Do
  | Read
  | Write
  | Prog
  | Begin
  | End
[@@deriving show, eq, sexp]
