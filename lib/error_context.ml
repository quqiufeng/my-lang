(** 错误上下文与诊断系统

    提供 Rust 风格的多行诊断输出：
    ```
    error: 类型错误
     --> file.ml:1:5
      |
    1 | let x = 1 + "hello"
      |             ^^^^^^^ 期望 int，但得到 string
    ```
*)

open Core

type severity =
  | Error
  | Warning
  | Note

type diagnostic = {
  severity : severity;
  message : string;
  file : string;
  line : int;
  col : int;
  source_line : string option;
  highlight_len : int;
  help : string option;
}

type error_context = {
  mutable diagnostics : diagnostic list;
}

let create () = { diagnostics = [] }

let add_diagnostic ctx ~severity ~message ?file ?(line=0) ?(col=0) ?source_line ?(highlight_len=1) ?help () =
  let d = {
    severity;
    message;
    file = Option.value file ~default:"";
    line;
    col;
    source_line;
    highlight_len;
    help;
  } in
  ctx.diagnostics <- d :: ctx.diagnostics

let severity_string = function
  | Error -> "error"
  | Warning -> "warning"
  | Note -> "note"

let severity_color = function
  | Error -> "\027[31m"    (* red *)
  | Warning -> "\027[33m" (* yellow *)
  | Note -> "\027[34m"    (* blue *)

let reset_color = "\027[0m"

(** 格式化单个诊断 *)
let format_diagnostic (d : diagnostic) =
  let buf = Buffer.create 512 in
  let emit line = Buffer.add_string buf (line ^ "\n") in
  
  (* 头部: error: message *)
  emit (Printf.sprintf "%s%s:%s %s" (severity_color d.severity) (severity_string d.severity) reset_color d.message);
  
  (* 位置:  --> file.ml:1:5 *)
  if not (String.equal d.file "") then
    emit (Printf.sprintf " --> %s:%d:%d" d.file d.line d.col);
  
  (* 源码片段 *)
  (match d.source_line with
   | Some line ->
       let line_num = string_of_int d.line in
       let padding = String.make (String.length line_num) ' ' in
       emit (Printf.sprintf " %s |" padding);
       emit (Printf.sprintf "%s | %s" line_num line);
       
       (* 高亮标记 *)
       let prefix = String.make (d.col - 1) ' ' in
       let markers = String.make d.highlight_len '^' in
       let help_suffix = match d.help with
         | Some h -> " " ^ h
         | None -> ""
       in
       emit (Printf.sprintf " %s | %s\027[32m%s%s\027[0m" padding prefix markers help_suffix)
   | None -> ());
  
  Buffer.contents buf

(** 格式化所有诊断 *)
let format_all ctx =
  ctx.diagnostics
  |> List.rev
  |> List.map ~f:format_diagnostic
  |> String.concat ~sep:"\n"

(** 从 Lexing.position 创建诊断 *)
let from_lexbuf ~severity ~message ?help lexbuf =
  let open Lexing in
  let pos = lexbuf.lex_curr_p in
  {
    severity;
    message;
    file = pos.pos_fname;
    line = pos.pos_lnum;
    col = pos.pos_cnum - pos.pos_bol + 1;
    source_line = None;
    highlight_len = 1;
    help;
  }

(** 提取 lexbuf 当前行的源码 *)
let extract_source_line lexbuf =
  let open Lexing in
  let pos = lexbuf.lex_curr_p in
  let line_start = pos.pos_bol in
  let content = lexbuf.lex_buffer in
  let len = lexbuf.lex_buffer_len in
  let rec find_end idx =
    if idx >= len || Char.equal (Bytes.get content idx) '\n' then idx
    else find_end (idx + 1)
  in
  let line_end = find_end line_start in
  if line_end > line_start then
    Some (Bytes.sub content ~pos:line_start ~len:(line_end - line_start) |> Bytes.to_string)
  else
    None

(** 创建完整的错误报告（从异常） *)
let from_exception ?(file="") exn lexbuf_opt =
  let severity = Error in
  let message, line, col, source_line, highlight_len =
    match exn with
    | Lexer.SyntaxError msg ->
        (msg, 0, 0, None, 1)
    | Parser.Error ->
        ("Parse error", 0, 0, None, 1)
    | Types.TypeError msg ->
        ("Type error: " ^ msg, 0, 0, None, 1)
    | Eval.RuntimeError (msg, pos_opt) ->
        ("Runtime error: " ^ msg,
         (match pos_opt with Some p -> p.line | None -> 0),
         (match pos_opt with Some p -> p.col | None -> 0),
         None, 1)
    | Vm.VMError msg ->
        ("VM error: " ^ msg, 0, 0, None, 1)
    | Ownership.OwnershipError msg ->
        ("Ownership error: " ^ msg, 0, 0, None, 1)
    | exn ->
        ("Error: " ^ Exn.to_string exn, 0, 0, None, 1)
  in
  
  let d = {
    severity;
    message;
    file;
    line;
    col;
    source_line;
    highlight_len;
    help = None;
  } in
  
  (* 尝试从 lexbuf 获取更精确的位置和源码 *)
  match lexbuf_opt with
  | Some lexbuf ->
      let open Lexing in
      let pos = lexbuf.lex_curr_p in
      let d' = {
        d with
        file = pos.pos_fname;
        line = pos.pos_lnum;
        col = max 1 (pos.pos_cnum - pos.pos_bol);
        source_line = extract_source_line lexbuf;
        highlight_len = max 1 (String.length (Lexing.lexeme lexbuf));
      } in
      d'
  | None -> d
