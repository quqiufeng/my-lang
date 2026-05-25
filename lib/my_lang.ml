(** MyLang 语言库入口 *)

module Ast = Ast
module Eval = Eval

let parse (s : string) : Ast.expr =
  let lexbuf = Lexing.from_string s in
  Parser.prog Lexer.read lexbuf

let eval (e : Ast.expr) : Ast.value = Eval.run e

let run (s : string) : Ast.value = s |> parse |> eval

let run_exn s =
  match run s with
  | v -> Ok v
  | exception Lexer.SyntaxError msg -> Error ("Syntax error: " ^ msg)
  | exception Parser.Error -> Error "Parse error"
  | exception Eval.RuntimeError msg -> Error ("Runtime error: " ^ msg)
