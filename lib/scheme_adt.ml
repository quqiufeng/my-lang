(** ADT 到 Chez Scheme 的高效编译模块

    将 MyLang 的 ADT 编译为 Chez Scheme 的 define-record-type，
    提供高效的模式匹配和构造函数支持。
*)

open Ast

(** 生成 ADT 类型定义的 Scheme 代码 *)
let compile_adt_type (type_name : string) (type_params : string list) (ctors : ctor_def list) : string =
  (* 为每个构造函数生成 define-record-type *)
  let ctor_defs = List.map (fun (ctor_name, arg_type, _) ->
    let ctor_name_lower = String.lowercase_ascii ctor_name in
    match arg_type with
    | None ->
        (* 无参数构造函数 *)
        Printf.sprintf
          "(define-record-type %s\n  (fields\n    (immutable tag)))\n\n(define %s-instance (make-%s '%s))"
          ctor_name_lower
          ctor_name_lower
          ctor_name_lower
          ctor_name_lower
    | Some _ ->
        (* 有参数构造函数 *)
        Printf.sprintf
          "(define-record-type %s\n  (fields\n    (immutable value)))"
          ctor_name_lower
  ) ctors in
  
  String.concat "\n\n" ctor_defs

(** 生成构造函数调用的 Scheme 代码 *)
let compile_ctor_call (ctor_name : string) (arg : expr option) (compile_expr : expr -> string) : string =
  let ctor_name_lower = String.lowercase_ascii ctor_name in
  match arg with
  | None ->
      (* 无参数构造函数 - 使用单例 *)
      Printf.sprintf "%s-instance" ctor_name_lower
  | Some e ->
      (* 有参数构造函数 - 使用 make-xxx *)
      Printf.sprintf "(make-%s %s)" ctor_name_lower (compile_expr e)

(** 生成模式匹配的 Scheme 代码（优化版） *)
let compile_pattern_match_optimized 
    (matched_expr : string) 
    (cases : (pattern * expr) list) 
    (compile_expr : expr -> string) 
    (scheme_of_pattern : pattern -> string) : string =
  
  let compile_case (pattern, body) =
    match pattern with
    | PWildcard ->
        Printf.sprintf "(else %s)" (compile_expr body)
    | PVar x ->
        Printf.sprintf "(else (let ((%s %s)) %s))" x matched_expr (compile_expr body)
    | PCtor (ctor_name, None) ->
        (* 无参数构造函数匹配 *)
        let ctor_name_lower = String.lowercase_ascii ctor_name in
        Printf.sprintf "((%s? %s) %s)" 
          ctor_name_lower
          matched_expr
          (compile_expr body)
    | PCtor (ctor_name, Some sub_pattern) ->
        (* 有参数构造函数匹配 *)
        let ctor_name_lower = String.lowercase_ascii ctor_name in
        let var_name = match sub_pattern with
          | PVar x -> x
          | _ -> Printf.sprintf "_ctor_arg_%s" ctor_name_lower
        in
        Printf.sprintf "((%s? %s) (let ((%s (%s-value %s))) %s))"
          ctor_name_lower
          matched_expr
          var_name
          ctor_name_lower
          matched_expr
          (compile_expr body)
    | _ ->
        (* 其他模式使用 equal? *)
        Printf.sprintf "((equal? %s %s) %s)"
          matched_expr
          (scheme_of_pattern pattern)
          (compile_expr body)
  in
  
  let clauses = List.map compile_case cases in
  Printf.sprintf "(cond %s)" (String.concat " " clauses)
