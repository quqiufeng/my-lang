(** 语言开发底座 - 公共类型和工具

    所有语言共享的基础类型和工具函数。
*)

(** 源码位置 *)
type pos = { line : int; col : int }

let string_of_pos p = Printf.sprintf "%d:%d" p.line p.col

(** 通用错误类型 *)
exception SyntaxError of string
exception ParseError of string
exception TypeError of string
exception RuntimeError of string * pos option
exception CompileError of string

(** 将异常转为字符串 *)
let string_of_error = function
  | SyntaxError msg -> "Syntax error: " ^ msg
  | ParseError msg -> "Parse error: " ^ msg
  | TypeError msg -> "Type error: " ^ msg
  | RuntimeError (msg, Some p) -> "Runtime error at " ^ string_of_pos p ^ ": " ^ msg
  | RuntimeError (msg, None) -> "Runtime error: " ^ msg
  | CompileError msg -> "Compile error: " ^ msg
  | exn -> "Unknown error: " ^ Printexc.to_string exn
