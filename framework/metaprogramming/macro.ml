(** 宏展开器实现

    基于通用 AST 类型实现宏定义和展开。
*)

open Core
open Ast_types

(** 宏定义 *)
type macro_def = {
  name : string;
  params : string list;
  expand : expr list -> expr;
}

(** 宏环境 *)
type macro_env = (string, macro_def) Hashtbl.t

(** 创建空的宏环境 *)
let empty_env () : macro_env = Hashtbl.create (module String)

(** 注册宏 *)
let define_macro env macro =
  Hashtbl.set env ~key:macro.name ~data:macro

(** 查找宏 *)
let find_macro env name =
  Hashtbl.find env name

(** 检查表达式是否是宏调用 *)
let is_macro_call env = function
  | EMacro (name, _) -> Option.is_some (find_macro env name)
  | _ -> false

(** 单步展开一个宏调用 *)
let expand_once env expr =
  match expr with
  | EMacro (name, args) ->
      (match find_macro env name with
       | Some macro ->
           if List.length args <> List.length macro.params then
             raise (Pos.CompileError (Printf.sprintf "Macro %s expects %d arguments, got %d"
               name (List.length macro.params) (List.length args)))
           else
             macro.expand args
       | None -> expr)
  | _ -> expr

(** 递归展开所有宏 *)
let rec expand_macros env expr =
  let expanded = match expr with
    | EMacro (name, args) ->
        let expanded_args = List.map args ~f:(expand_macros env) in
        (match find_macro env name with
         | Some macro ->
             if List.length expanded_args <> List.length macro.params then
               raise (Pos.CompileError (Printf.sprintf "Macro %s expects %d arguments, got %d"
                 name (List.length macro.params) (List.length expanded_args)))
             else
               macro.expand expanded_args
         | None -> EMacro (name, expanded_args))
    | EBinary (op, e1, e2) -> EBinary (op, expand_macros env e1, expand_macros env e2)
    | EUnary (op, e) -> EUnary (op, expand_macros env e)
    | ELet (x, v, body) -> ELet (x, expand_macros env v, expand_macros env body)
    | ELetRec (x, v, body) -> ELetRec (x, expand_macros env v, expand_macros env body)
    | EAssign (e1, e2) -> EAssign (expand_macros env e1, expand_macros env e2)
    | EIf (c, t, f) -> EIf (expand_macros env c, expand_macros env t, expand_macros env f)
    | EMatch (e, cases) -> EMatch (expand_macros env e,
        List.map cases ~f:(fun (p, body) -> (p, expand_macros env body)))
    | EWhile (c, b) -> EWhile (expand_macros env c, expand_macros env b)
    | ESeq (e1, e2) -> ESeq (expand_macros env e1, expand_macros env e2)
    | EFun (params, body) -> EFun (params, expand_macros env body)
    | EApp (f, args) -> EApp (expand_macros env f, List.map args ~f:(expand_macros env))
    | EList es -> EList (List.map es ~f:(expand_macros env))
    | ETuple es -> ETuple (List.map es ~f:(expand_macros env))
    | ERecord fields -> ERecord (List.map fields ~f:(fun (k, e) -> (k, expand_macros env e)))
    | EArray es -> EArray (List.map es ~f:(expand_macros env))
    | ERef e -> ERef (expand_macros env e)
    | EDeref e -> EDeref (expand_macros env e)
    | ETry (e, cases) -> ETry (expand_macros env e,
        List.map cases ~f:(fun (p, body) -> (p, expand_macros env body)))
    | ERaise e -> ERaise (expand_macros env e)
    | EAnnot (e, t) -> EAnnot (expand_macros env e, t)
    | EQuote e -> EQuote (expand_macros env e)
    | EAntiQuote e -> EAntiQuote (expand_macros env e)
    | EModule (name, body) -> EModule (name, expand_macros env body)
    | EDot (e, field) -> EDot (expand_macros env e, field)
    | e -> e
  in
  (* 如果展开后还有宏，继续展开 *)
  if Quote.exists (is_macro_call env) expanded then
    expand_macros env expanded
  else
    expanded

(** 检查 AST 中是否还有未展开的宏 *)
let has_macros env expr =
  Quote.exists (is_macro_call env) expr

(** 内置宏示例 *)

(** unless 宏：unless cond body = if not cond then body else () *)
let unless_macro = {
  name = "unless";
  params = ["cond"; "body"];
  expand = (function
    | [cond; body] -> EIf (EUnary (Not, cond), body, ELit LUnit)
    | _ -> raise (Pos.CompileError "unless macro expects 2 arguments"))
}

(** when 宏：when cond body = if cond then body else () *)
let when_macro = {
  name = "when";
  params = ["cond"; "body"];
  expand = (function
    | [cond; body] -> EIf (cond, body, ELit LUnit)
    | _ -> raise (Pos.CompileError "when macro expects 2 arguments"))
}

(** cond 宏：多分支条件 *)
let cond_macro = {
  name = "cond";
  params = [];  (* 变参 *)
  expand = (fun clauses ->
    match clauses with
    | [] -> ELit LUnit
    | clauses ->
        let rec build = function
          | [] -> ELit LUnit
          | [e] -> (match e with
              | ETuple [cond; body] | EList [cond; body] -> EIf (cond, body, ELit LUnit)
              | _ -> raise (Pos.CompileError "cond clause must be (cond, body)"))
          | e :: rest -> (match e with
              | ETuple [cond; body] | EList [cond; body] -> EIf (cond, body, build rest)
              | _ -> raise (Pos.CompileError "cond clause must be (cond, body)"))
        in
        build clauses)
}

(** -> 箭头宏：创建 lambda (x -> x + 1) 的语法糖 *)
let arrow_macro = {
  name = "->";
  params = ["params"; "body"];
  expand = (function
    | [params; body] -> (match params with
        | EList ps | ETuple ps -> EFun (List.map ps ~f:(function EVar x -> x | _ -> raise (Pos.CompileError "-> params must be variables")), body)
        | EVar x -> EFun ([x], body)
        | _ -> raise (Pos.CompileError "-> macro expects params and body"))
    | _ -> raise (Pos.CompileError "-> macro expects 2 arguments"))
}

(** 创建包含常用宏的环境 *)
let builtin_macros () =
  let env = empty_env () in
  define_macro env unless_macro;
  define_macro env when_macro;
  define_macro env cond_macro;
  define_macro env arrow_macro;
  env
