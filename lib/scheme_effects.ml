(** 代数效果（Algebraic Effects）模块

    利用 Chez Scheme 的 call/cc 或 shift/reset 实现代数效果，
    支持无感异步 I/O、可定制异常处理、状态隔离沙箱等。
*)

open Ast

(** 效果定义 *)
type effect_def = {
  name : string;                    (* 效果名 *)
  operations : effect_op list;      (* 效果操作 *)
}

(** 效果操作 *)
and effect_op = {
  op_name : string;                 (* 操作名 *)
  op_params : string list;          (* 参数名列表 *)
  op_body : Ast.expr option;        (* 可选的操作体 *)
}

(** 生成效果定义的 Scheme 代码 *)
let compile_effect_def (name : string) (ops : effect_op list) : string =
  let ops_code = List.map (fun op ->
    let params_str = String.concat " " op.op_params in
    Printf.sprintf "    (immutable %s-handler)" op.op_name
  ) ops in
  
  Printf.sprintf
    "(define-record-type %s-effect\n  (fields\n    %s))"
    (String.lowercase_ascii name)
    (String.concat "\n    " ops_code)

(** 生成 perform 操作的 Scheme 代码 *)
let compile_perform (op_name : string) (arg : Ast.expr) (compile_expr : Ast.expr -> string) : string =
  Printf.sprintf
    "(call/cc (lambda (k) (effect-handler '%s %s k)))"
    (String.lowercase_ascii op_name)
    (compile_expr arg)

(** 生成 handle 块的 Scheme 代码 *)
let compile_handle 
    (body : Ast.expr) 
    (handlers : (string * string * string * Ast.expr) list) 
    (compile_expr : Ast.expr -> string) : string =
  
  let handler_defs = List.map (fun (op_name, param, cont_var, handler_body) ->
    Printf.sprintf
      "(define (handle-%s %s %s) %s)"
      (String.lowercase_ascii op_name)
      param
      cont_var
      (compile_expr handler_body)
  ) handlers in
  
  let handler_table = List.map (fun (op_name, _, _, _) ->
    Printf.sprintf "'(%s . handle-%s)" 
      (String.lowercase_ascii op_name)
      (String.lowercase_ascii op_name)
  ) handlers in
  
  Printf.sprintf
    "(let ()\n  %s\n  (define effect-handler\n    (lambda (op arg k)\n      (case op\n        %s\n        (else (error 'perform \"unhandled effect\" op)))))\n  %s)"
    (String.concat "\n  " handler_defs)
    (String.concat "\n        " (List.map (fun (op_name, _, _, _) ->
      Printf.sprintf "((%s) (handle-%s arg k))"
        (String.lowercase_ascii op_name)
        (String.lowercase_ascii op_name)
    ) handlers))
    (compile_expr body)

(** 使用 shift/reset 实现的代数效果 *)
let compile_with_shift_reset 
    (body : Ast.expr) 
    (handlers : (string * string * string * Ast.expr) list) 
    (compile_expr : Ast.expr -> string) : string =
  
  Printf.sprintf
    "(import (chezscheme))\n;; 需要 shift/reset 库\n(define-syntax perform\n  (syntax-rules ()\n    ((_ op arg)\n     (shift k (list 'op arg k)))))\n\n(define-syntax handle\n  (syntax-rules ()\n    ((_ body ((op param cont) handler-body) ...)\n     (reset\n       (let ((result body))\n         result)))))\n\n%s"
    (compile_expr body)

(** 使用动态绑定实现代数效果（简化版） *)
let compile_effect_simple 
    (effect_name : string) 
    (ops : effect_op list) 
    (body : Ast.expr) 
    (compile_expr : Ast.expr -> string) : string =
  
  let effect_handler_var = Printf.sprintf "%s-handler" (String.lowercase_ascii effect_name) in
  
  let op_defs = List.map (fun op ->
    let params_str = String.concat " " op.op_params in
    Printf.sprintf
      "(define (%s %s)\n  (if %s\n      (%s '%s %s)\n      (error '%s \"effect not handled\")))"
      (String.lowercase_ascii op.op_name)
      params_str
      effect_handler_var
      effect_handler_var
      (String.lowercase_ascii op.op_name)
      params_str
      (String.lowercase_ascii op.op_name)
  ) ops in
  
  Printf.sprintf
    "(let ((%s #f))\n  %s\n  (define (with-handler handler thunk)\n    (set! %s handler)\n    (let ((result (thunk)))\n      (set! %s #f)\n      result))\n  %s)"
    effect_handler_var
    (String.concat "\n  " op_defs)
    effect_handler_var
    effect_handler_var
    (compile_expr body)
