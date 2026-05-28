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

(** 将二元运算符转换为 Scheme *)
let scheme_of_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "quotient"
  | Mod -> "modulo"
  | Eq -> "equal?"
  | Neq -> (fun a b -> Printf.sprintf "(not (equal? %s %s))" a b)
  | Lt -> "<"
  | Le -> "<="
  | Gt -> ">"
  | Ge -> ">="
  | And -> "and"
  | Or -> "or"
  | Cons -> "cons"
  | Concat -> "string-append"

(** 将一元运算符转换为 Scheme *)
let scheme_of_unop = function
  | Neg -> "-"
  | Not -> "not"

(** 将模式转换为 Scheme match 子句 *)
let rec scheme_of_pattern = function
  | PWildcard -> "_"
  | PVar x -> x
  | PInt n -> string_of_int n
  | PBool b -> if b then "#t" else "#f"
  | PString s -> Printf.sprintf "%S" s
  | PChar c -> Printf.sprintf "#\\%c" c
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
  | EUnit -> "(void)"
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
  | EBinary (op, e1, e2) ->
      let s1 = compile_expr e1 in
      let s2 = compile_expr e2 in
      (match op with
       | Neq -> Printf.sprintf "(not (equal? %s %s))" s1 s2
       | Concat -> Printf.sprintf "(string-append %s %s)" s1 s2
       | _ -> Printf.sprintf "(%s %s %s)" (scheme_of_binop_simple op) s1 s2)
  | EUnary (op, e) ->
      let s = compile_expr e in
      (match op with
       | Neg -> Printf.sprintf "(- %s)" s
       | Not -> Printf.sprintf "(not %s)" s)
  | ELet (x, e1, e2) ->
      Printf.sprintf "(let ((%s %s)) %s)" x (compile_expr e1) (compile_expr e2)
  | ELetRec (x, e1, e2) ->
      Printf.sprintf "(letrec ((%s %s)) %s)" x (compile_expr e1) (compile_expr e2)
  | EFun (param, body) ->
      Printf.sprintf "(lambda (%s) %s)" param (compile_expr body)
  | EApp (f, arg) ->
      Printf.sprintf "(%s %s)" (compile_expr f) (compile_expr arg)
  | EIf (cond, then_, else_) ->
      Printf.sprintf "(if %s %s %s)"
        (compile_expr cond) (compile_expr then_) (compile_expr else_)
  | EMatch (e, cases) ->
      let s = compile_expr e in
      let clauses = List.map compile_match_case cases in
      Printf.sprintf "(match %s %s)" s (String.concat " " clauses)
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
      let handlers = List.map compile_try_case cases in
      Printf.sprintf "(guard (exn %s) %s)"
        (String.concat " " handlers) s
  | ECtor (name, None) -> Printf.sprintf "'%s" name
  | ECtor (name, Some e) ->
      Printf.sprintf "(list '%s %s)" name (compile_expr e)
  | ETypeDef _ -> "(void)"
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
  | ETraitDef _ -> "(void)"
  | ETraitImpl _ -> "(void)"
  | EEffectDef _ -> "(void)"
  | EPerform _ -> "(void)"
  | EHandle _ -> "(void)"
  | ESpawn _ -> "(void)"
  | ESend _ -> "(void)"
  | EReceive -> "(void)"

and scheme_of_binop_simple = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "quotient"
  | Mod -> "modulo"
  | Eq -> "equal?"
  | Neq -> "nequal?"
  | Lt -> "<"
  | Le -> "<="
  | Gt -> ">"
  | Ge -> ">="
  | And -> "and"
  | Or -> "or"
  | Cons -> "cons"
  | Concat -> "string-append"

and compile_match_case (pattern, body) =
  Printf.sprintf "(%s %s)" (scheme_of_pattern pattern) (compile_expr body)

and compile_try_case (pattern, body) =
  Printf.sprintf "(%s %s)" (scheme_of_pattern pattern) (compile_expr body)

(** 编译整个程序 *)
let compile_program expr =
  let scheme_code = compile_expr expr in
  Printf.sprintf "(import (chezscheme))\n(display %s)\n(newline)\n" scheme_code

(** 写入 Scheme 文件 *)
let write_scheme_file filename expr =
  let code = compile_program expr in
  let oc = open_out filename in
  output_string oc code;
  close_out oc

(** 使用 Chez Scheme 编译并执行 *)
let compile_and_run expr =
  let temp_file = Filename.temp_file "mylang" ".ss" in
  let output_file = Filename.temp_file "mylang" ".so" in
  write_scheme_file temp_file expr;
  
  (* 编译 *)
  let compile_cmd = Printf.sprintf "/opt/ChezScheme/ta6le/bin/ta6le/scheme --compile %s" temp_file in
  let compile_result = Sys.command compile_cmd in
  
  if compile_result <> 0 then
    Error "Scheme compilation failed"
  else
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
