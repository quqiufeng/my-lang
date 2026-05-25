(** MyLang 语言库入口 *)

module Ast = Ast
module Eval = Eval

let parse (s : string) : Ast.expr =
  let lexbuf = Lexing.from_string s in
  Parser.prog Lexer.read lexbuf

let eval (e : Ast.expr) : Ast.value = Eval.run e

let run (s : string) : Ast.value = s |> parse |> eval

let run_exn s =
  try Ok (run s) with
  | Lexer.SyntaxError msg -> Error ("Syntax error: " ^ msg)
  | Parser.Error -> Error "Parse error"
  | Eval.RuntimeError msg -> Error ("Runtime error: " ^ msg)
