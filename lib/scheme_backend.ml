(** Chez Scheme 后端

    将 MyLang AST 编译为 Scheme 代码，然后使用 Chez Scheme 编译为机器码。
*)

open Ast

(** 将 MyLang 值转换为 Scheme 表达式 *)
let rec scheme_of_value = function
  | VInt n -> string_of_int n
  | VBool b -> if b then "#t" else "#f"
  | VChar c -> Printf.sprintf "#\\%c" c
  | VString s -> Printf.sprintf "%S" s
  | VUnit -> "(void)"
  | VList vs ->
      let elems = List.map scheme_of_value vs in
      Printf.sprintf "'(%s)" (String.concat " " elems)
  | VTuple vs ->
      let elems = List.map scheme_of_value vs in
      Printf.sprintf "(vector %s)" (String.concat " " elems)
  | VFun _ -> "#<procedure>"
  | VBuiltin (name, _) -> Printf.sprintf "#<builtin:%s>" name
  | VCtor (name, None) -> Printf.sprintf "'%s" name
  | VCtor (name, Some v) ->
      Printf.sprintf "(list '%s %s)" name (scheme_of_value v)
  | VRef r -> Printf.sprintf "(box %s)" (scheme_of_value !r)
  | VArray arr ->
      let elems = Array.to_list arr |> List.map scheme_of_value in
      Printf.sprintf "(vector %s)" (String.concat " " elems)
  | VRecord fields ->
      let pairs = List.map (fun (k, r) ->
        Printf.sprintf "(cons '%s %s)" k (scheme_of_value !r)
      ) fields in
      Printf.sprintf "(list %s)" (String.concat " " pairs)
  | VModule (name, _) -> Printf.sprintf "#<module:%s>" name
  | VExn (name, _) -> Printf.sprintf "#<exception:%s>" name

(** 将模式转换为 Scheme match 子句 *)
let rec scheme_of_pattern = function
  | PWildcard -> "_"
  | PVar x -> x
  | PInt n -> string_of_int n
  | PBool b -> if b then "#t" else "#f"
  | PString s -> Printf.sprintf "%S" s
  | PUnit -> "(void)"
  | PList ps ->
      let elems = List.map scheme_of_pattern ps in
      Printf.sprintf "'(%s)" (String.concat " " elems)
  | PTuple ps ->
      let elems = List.map scheme_of_pattern ps in
      Printf.sprintf "#(%s)" (String.concat " " elems)
  | PCons (p1, p2) ->
      Printf.sprintf "(cons %s %s)" (scheme_of_pattern p1) (scheme_of_pattern p2)
  | PCtor (name, None) -> Printf.sprintf "'%s" name
  | PCtor (name, Some p) ->
      Printf.sprintf "(list '%s %s)" name (scheme_of_pattern p)
  | PRecord _ -> "_"  (* 记录模式简化处理 *)

(** 将 MyLang AST 转换为 Scheme 代码 *)
let rec compile_expr = function
  | EInt n -> string_of_int n
  | EBool b -> if b then "#t" else "#f"
  | EChar c -> Printf.sprintf "#\\%c" c
  | EString s -> Printf.sprintf "%S" s
  | EVar x -> x
  | EList es ->
      let elems = List.map compile_expr es in
      Printf.sprintf "(list %s)" (String.concat " " elems)
  | ETuple es ->
      let elems = List.map compile_expr es in
      Printf.sprintf "(vector %s)" (String.concat " " elems)
  | EArray es ->
      let elems = List.map compile_expr es in
      Printf.sprintf "(vector %s)" (String.concat " " elems)
  | ERecord fields ->
      let pairs = List.map (fun (k, e) ->
        Printf.sprintf "(cons '%s %s)" k (compile_expr e)
      ) fields in
      Printf.sprintf "(list %s)" (String.concat " " pairs)
  | EAdd (e1, e2) ->
      Printf.sprintf "(+ %s %s)" (compile_expr e1) (compile_expr e2)
  | ESub (e1, e2) ->
      Printf.sprintf "(- %s %s)" (compile_expr e1) (compile_expr e2)
  | EMul (e1, e2) ->
      Printf.sprintf "(* %s %s)" (compile_expr e1) (compile_expr e2)
  | EDiv (e1, e2) ->
      Printf.sprintf "(quotient %s %s)" (compile_expr e1) (compile_expr e2)
  | EEq (e1, e2) ->
      Printf.sprintf "(equal? %s %s)" (compile_expr e1) (compile_expr e2)
  | ENeq (e1, e2) ->
      Printf.sprintf "(not (equal? %s %s))" (compile_expr e1) (compile_expr e2)
  | ELt (e1, e2) ->
      Printf.sprintf "(< %s %s)" (compile_expr e1) (compile_expr e2)
  | ELe (e1, e2) ->
      Printf.sprintf "(<= %s %s)" (compile_expr e1) (compile_expr e2)
  | EGt (e1, e2) ->
      Printf.sprintf "(> %s %s)" (compile_expr e1) (compile_expr e2)
  | EGe (e1, e2) ->
      Printf.sprintf "(>= %s %s)" (compile_expr e1) (compile_expr e2)
  | EAnd (e1, e2) ->
      Printf.sprintf "(and %s %s)" (compile_expr e1) (compile_expr e2)
  | EOr (e1, e2) ->
      Printf.sprintf "(or %s %s)" (compile_expr e1) (compile_expr e2)
  | ENot e ->
      Printf.sprintf "(not %s)" (compile_expr e)
  | ELet (x, e1, e2) ->
      Printf.sprintf "(let ((%s %s)) %s)" x (compile_expr e1) (compile_expr e2)
  | ELetRec (x, e1, e2) ->
      Printf.sprintf "(letrec ((%s %s)) %s)" x (compile_expr e1) (compile_expr e2)
  | EFun (param, body) ->
      Printf.sprintf "(lambda (%s) %s)" param (compile_expr body)
  | EApp (ECtor (name, None), arg) ->
      (* 构造函数应用：Some 42 -> (make-some 42) *)
      Printf.sprintf "(make-%s %s)" (String.lowercase_ascii name) (compile_expr arg)
  | EApp (f, arg) ->
      Printf.sprintf "(%s %s)" (compile_expr f) (compile_expr arg)
  | EIf (cond, then_, else_) ->
      Printf.sprintf "(if %s %s %s)"
        (compile_expr cond) (compile_expr then_) (compile_expr else_)
  | EMatch (e, cases) ->
      let s = compile_expr e in
      Scheme_adt.compile_pattern_match_optimized s cases compile_expr scheme_of_pattern
  | ESeq (e1, e2) ->
      Printf.sprintf "(begin %s %s)" (compile_expr e1) (compile_expr e2)
  | EWhile (cond, body) ->
      Printf.sprintf "(let loop () (when %s %s (loop)))"
        (compile_expr cond) (compile_expr body)
  | ECons (e1, e2) ->
      Printf.sprintf "(cons %s %s)" (compile_expr e1) (compile_expr e2)
  | ECat (e1, e2) ->
      Printf.sprintf "(string-append %s %s)" (compile_expr e1) (compile_expr e2)
  | EIndex (e1, e2) ->
      Printf.sprintf "(list-ref %s %s)" (compile_expr e1) (compile_expr e2)
  | ESlice (e, start, end_) ->
      let s = compile_expr e in
      let start_s = match start with Some e -> compile_expr e | None -> "0" in
      let end_s = match end_ with Some e -> compile_expr e | None -> Printf.sprintf "(length %s)" s in
      Printf.sprintf "(list-take (list-drop %s %s) (- %s %s))" s start_s end_s start_s
  | ERef e -> Printf.sprintf "(box %s)" (compile_expr e)
  | EDeref e -> Printf.sprintf "(unbox %s)" (compile_expr e)
  | EAssign (e1, e2) ->
      Printf.sprintf "(set-box! %s %s)" (compile_expr e1) (compile_expr e2)
  | ERaise e ->
      Printf.sprintf "(raise %s)" (compile_expr e)
  | ETry (e, cases) ->
      let s = compile_expr e in
      let handlers = List.map (fun (pattern, body) ->
        Printf.sprintf "(%s %s)" (scheme_of_pattern pattern) (compile_expr body)
      ) cases in
      Printf.sprintf "(guard (exn %s) %s)"
        (String.concat " " handlers) s
  | ECtor (name, None) -> Scheme_adt.compile_ctor_call name None compile_expr
  | ECtor (name, Some e) -> Scheme_adt.compile_ctor_call name (Some e) compile_expr
  | ETypeDef (name, params, ctors) -> 
      (* 类型定义返回 void，实际定义在 compile_program 中处理 *)
      "(void)"
  | EAnnot (e, _) -> compile_expr e
  | ERange (start, end_) ->
      Printf.sprintf "(let ((s %s) (e %s)) (let loop ((i s) (acc '())) (if (> i e) (reverse acc) (loop (+ i 1) (cons i acc)))))"
        (compile_expr start) (compile_expr end_)
  | ERecordGet (e, field) ->
      Printf.sprintf "(cdr (assq '%s %s))" field (compile_expr e)
  | ERecordUpdate (e, fields) ->
      let s = compile_expr e in
      let updates = List.map (fun (k, v) ->
        Printf.sprintf "(cons '%s %s)" k (compile_expr v)
      ) fields in
      Printf.sprintf "(append (list %s) %s)" (String.concat " " updates) s
  | EModule (name, body) ->
      Printf.sprintf "(define-module %s %s)" name (compile_expr body)
  | EModuleType _ -> "(void)"
  | EOpen name -> Printf.sprintf "(import %s)" name
  | EDot (e, field) ->
      Printf.sprintf "(cdr (assq '%s %s))" field (compile_expr e)
  | ETraitDef (name, _params, methods) ->
      (* 生成 trait 定义：在 Scheme 中表现为记录类型 *)
      let method_decls = List.map (fun (mname, _mtype) ->
        Printf.sprintf "(define (%s-%s self) (self '%s))" 
          (String.lowercase_ascii name) mname mname
      ) methods in
      Printf.sprintf ";; trait %s\n%s" name (String.concat "\n" method_decls)
  | ETraitImpl (trait_name, type_name, methods) ->
      (* 生成 trait 实现：为特定类型注册方法 *)
      let method_bindings = List.map (fun (mname, mexpr) ->
        let scheme_body = compile_expr mexpr in
        Printf.sprintf "(hashtable-set! %s-method-table '%s-%s %s)"
          (String.lowercase_ascii trait_name)
          type_name mname
          scheme_body
      ) methods in
      Printf.sprintf ";; impl %s for %s\n%s"
        trait_name type_name
        (String.concat "\n" method_bindings)
  | EEffectDef (name, ops) ->
      let effect_ops = List.map (fun op ->
        { Scheme_effects.op_name = op; op_params = []; op_body = None }
      ) ops in
      Scheme_effects.compile_effect_def name effect_ops
  | EPerform (op, arg) ->
      Scheme_effects.compile_perform op arg compile_expr
  | EHandle (e, handlers) ->
      Scheme_effects.compile_handle e handlers compile_expr
  | ESpawn e -> Scheme_actor.compile_spawn e compile_expr
  | ESend (pid, msg) -> Scheme_actor.compile_send pid msg compile_expr
  | EReceive -> Scheme_actor.compile_receive ()

(** 编译模式匹配分支 *)
and compile_match_case s (pattern, body) =
  match pattern with
  | PWildcard -> Printf.sprintf "(else %s)" (compile_expr body)
  | _ -> Printf.sprintf "((equal? %s %s) %s)" s (scheme_of_pattern pattern) (compile_expr body)

(** 收集所有类型定义 *)
let rec collect_type_defs (expr : Ast.expr) : string list =
  match expr with
  | ETypeDef (name, params, ctors) -> 
      [Scheme_adt.compile_adt_type name params ctors]
  | ESeq (e1, e2) -> 
      (collect_type_defs e1) @ (collect_type_defs e2)
  | ELet (_, e1, e2) | ELetRec (_, e1, e2) ->
      (collect_type_defs e1) @ (collect_type_defs e2)
  | EIf (cond, then_, else_) ->
      (collect_type_defs cond) @ (collect_type_defs then_) @ (collect_type_defs else_)
  | EMatch (e, cases) ->
      (collect_type_defs e) @ (List.concat (List.map (fun (_, body) -> collect_type_defs body) cases))
  | _ -> []

(** 编译整个程序 *)
let compile_program expr =
  let type_defs = collect_type_defs expr in
  let type_defs_code = 
    if type_defs = [] then ""
    else (String.concat "\n\n" type_defs) ^ "\n\n"
  in
  let ffi_decls = Scheme_ffi.compile_stdlib_ffi () in
  let scheme_code = compile_expr expr in
  Printf.sprintf "(import (chezscheme))\n\n;; FFI 声明\n%s\n\n;; 类型定义\n%s\n;; 主程序\n(display %s)\n(newline)\n" 
    ffi_decls
    type_defs_code
    scheme_code

(** 写入 Scheme 文件 *)
let write_scheme_file filename expr =
  let code = compile_program expr in
  let oc = open_out filename in
  output_string oc code;
  close_out oc

(** 使用 Chez Scheme 编译并执行 *)
let compile_and_run expr =
  let temp_file = Filename.temp_file "mylang" ".ss" in
  write_scheme_file temp_file expr;
  
  (* 执行 *)
  let run_cmd = Printf.sprintf "/opt/ChezScheme/ta6le/bin/ta6le/scheme --program %s" temp_file in
  let run_result = Sys.command run_cmd in
  if run_result <> 0 then
    Error "Scheme execution failed"
  else
    Ok (VUnit)

(** 解释执行 Scheme 代码 *)
let interpret_scheme code =
  let temp_file = Filename.temp_file "mylang" ".ss" in
  let oc = open_out temp_file in
  output_string oc code;
  close_out oc;
  
  let cmd = Printf.sprintf "/opt/ChezScheme/ta6le/bin/ta6le/scheme --program %s" temp_file in
  let result = Sys.command cmd in
  if result = 0 then Ok (VUnit) else Error "Scheme execution failed"
