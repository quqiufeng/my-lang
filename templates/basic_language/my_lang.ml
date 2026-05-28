(** 基础语言模板 - 库入口 *)

module Ast = Ast
module Lexer = Lexer
module Parser = Parser
module Eval = Eval

(** 运行程序 *)
let run = Eval.run

(** 值转字符串 *)
let string_of_value = Ast.string_of_value

(** 表达式转字符串 *)
let string_of_expr = Ast.string_of_expr
