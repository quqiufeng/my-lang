(** 极简求值器 - 模板项目占位实现 *)

open Ast

let parse s =
  let lexbuf = Lexing.from_string s in
  Parser.program Lexer.token lexbuf

let eval _expr = VInt 0  (* 占位 *)

let run s =
  try
    let _prog = parse s in
    Ok (VInt 0)
  with
  | Lexer.LexError msg -> Error ("Lex error: " ^ msg)
  | Parser.Error -> Error "Parse error"
