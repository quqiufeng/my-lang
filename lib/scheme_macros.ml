(** 编译期元编程（宏）模块

    利用 Scheme 宏实现 MyLang 的编译期元编程，
    支持 comptime、宏展开、编译期计算。
*)

open Ast

(** 宏定义 *)
type macro_def = {
  name : string;                    (* 宏名 *)
  params : string list;             (* 参数列表 *)
  body : Ast.expr;                  (* 宏体 *)
}

(** 编译期上下文 *)
type compile_time_ctx = {
  macros : (string * macro_def) list;  (* 已定义的宏 *)
  constants : (string * Ast.expr) list;  (* 编译期常量 *)
}

(** 创建空的编译期上下文 *)
let empty_ctx () : compile_time_ctx = {
  macros = [];
  constants = [];
}

(** 辅助函数：字符串分割 *)
let string_split (sep : string) (s : string) : string list =
  let sep_len = String.length sep in
  let s_len = String.length s in
  if sep_len = 0 then [s]
  else if s_len < sep_len then [s]
  else
    let rec aux acc start =
      if start > s_len then List.rev acc
      else
        let found = ref (-1) in
        let i = ref start in
        while !found < 0 && !i <= s_len - sep_len do
          if String.sub s !i sep_len = sep then found := !i
          else i := !i + 1
        done;
        if !found >= 0 then
          let before = String.sub s start (!found - start) in
          aux (before :: acc) (!found + sep_len)
        else
          let rest = String.sub s start (s_len - start) in
          List.rev (rest :: acc)
    in
    aux [] 0

(** 生成宏定义的 Scheme 代码 *)
let compile_macro_def (name : string) (params : string list) (body : Ast.expr) (compile_expr : Ast.expr -> string) : string =
  let params_str = String.concat " " params in
  Printf.sprintf
    "(define-syntax %s\n  (syntax-rules ()\n    ((_ %s)\n     %s)))"
    (String.lowercase_ascii name)
    params_str
    (compile_expr body)

(** 生成 comptime 块的 Scheme 代码 *)
let compile_comptime (body : Ast.expr) (compile_expr : Ast.expr -> string) : string =
  (* comptime 块在编译期执行，结果作为常量内联 *)
  Printf.sprintf
    "(let-syntax ((comptime-result\n  (let ((result %s))\n    (syntax-rules ()\n      ((_) result)))))\n  (comptime-result))"
    (compile_expr body)

(** 生成编译期内联的 Scheme 代码 *)
let compile_inline (func : Ast.expr) (args : Ast.expr list) (compile_expr : Ast.expr -> string) : string =
  (* 使用 Chez Scheme 的 define-inline 或 let-syntax 实现内联 *)
  Printf.sprintf
    "(let-syntax ((inline-func\n  (syntax-rules ()\n    ((inline-func %s) %s))))\n  (inline-func %s))"
    (String.concat " " (List.mapi (fun i _ -> Printf.sprintf "arg%d" i) args))
    (compile_expr func)
    (String.concat " " (List.map compile_expr args))

(** 生成编译期常量折叠的 Scheme 代码 *)
let compile_const_fold (expr : Ast.expr) (compile_expr : Ast.expr -> string) : string =
  (* 尝试在编译期计算常量表达式 *)
  match expr with
  | EInt n -> string_of_int n
  | EBool b -> if b then "#t" else "#f"
  | EString s -> Printf.sprintf "%S" s
  | EAdd (EInt a, EInt b) -> string_of_int (a + b)
  | ESub (EInt a, EInt b) -> string_of_int (a - b)
  | EMul (EInt a, EInt b) -> string_of_int (a * b)
  | EDiv (EInt a, EInt b) when b <> 0 -> string_of_int (a / b)
  | _ -> compile_expr expr

(** 生成编译期字符串拼接 *)
let compile_string_concat (exprs : Ast.expr list) (compile_expr : Ast.expr -> string) : string =
  (* 尝试在编译期拼接字符串 *)
  let all_const = List.for_all (function EString _ -> true | _ -> false) exprs in
  if all_const then
    let result = String.concat "" (List.map (function EString s -> s | _ -> "") exprs) in
    Printf.sprintf "%S" result
  else
    Printf.sprintf "(string-append %s)" (String.concat " " (List.map compile_expr exprs))

(** 生成编译期列表操作 *)
let compile_list_comprehension 
    (var : string) 
    (iter_expr : Ast.expr) 
    (body_expr : Ast.expr) 
    (compile_expr : Ast.expr -> string) : string =
  Printf.sprintf
    "(map (lambda (%s) %s) %s)"
    var
    (compile_expr body_expr)
    (compile_expr iter_expr)

(** 生成编译期类型检查 *)
let compile_type_check (expr : Ast.expr) (expected_type : string) (compile_expr : Ast.expr -> string) : string =
  let type_pred = match expected_type with
    | "int" -> "integer?"
    | "float" -> "flonum?"
    | "string" -> "string?"
    | "bool" -> "boolean?"
    | "list" -> "list?"
    | "function" -> "procedure?"
    | _ -> (Printf.sprintf "%s?" expected_type)
  in
  Printf.sprintf
    "(let ((val %s))\n  (if (%s val)\n      val\n      (error 'type-check \"expected %s, got\" (type val))))"
    (compile_expr expr)
    type_pred
    expected_type

(** 生成编译期代码生成 *)
let compile_code_gen (template : string) (vars : (string * Ast.expr) list) (compile_expr : Ast.expr -> string) : string =
  (* 简单的模板替换 *)
  let result = List.fold_left (fun acc (var, expr) ->
    let placeholder = Printf.sprintf "{{%s}}" var in
    let value = compile_expr expr in
    String.concat value (string_split placeholder acc)
  ) template vars in
  result

(** 生成完整的宏展开程序 *)
let compile_with_macros 
    (macros : macro_def list) 
    (body : Ast.expr) 
    (compile_expr : Ast.expr -> string) : string =
  
  let macro_defs = List.map (fun m ->
    compile_macro_def m.name m.params m.body compile_expr
  ) macros in
  
  Printf.sprintf
    "(begin\n  %s\n  %s)"
    (String.concat "\n  " macro_defs)
    (compile_expr body)
