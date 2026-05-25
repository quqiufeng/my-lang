(** 编译时求值（CTFE）

    在编译阶段求值常量表达式，实现常量折叠。
*)

open Ast_types

(** 可编译时求值的值 *)
type ct_value =
  | CVInt of int
  | CVFloat of float
  | CVBool of bool
  | CVString of string
  | CVChar of char
  | CVUnit
  | CVList of ct_value list
  | CVTuple of ct_value list

(** 将 ct_value 转为 AST 字面量 *)
let ct_value_to_lit = function
  | CVInt n -> LInt n
  | CVFloat f -> LFloat f
  | CVBool b -> LBool b
  | CVString s -> LString s
  | CVChar c -> LChar c
  | CVUnit -> LUnit
  | CVList vs -> raise (Pos.CompileError "Cannot embed CT list as literal")
  | CVTuple vs -> raise (Pos.CompileError "Cannot embed CT tuple as literal")

(** 将 ct_value 转为 AST 表达式 *)
let ct_value_to_expr v =
  match v with
  | CVList vs -> EList (List.map (fun v -> ELit (ct_value_to_lit v)) vs)
  | CVTuple vs -> ETuple (List.map (fun v -> ELit (ct_value_to_lit v)) vs)
  | _ -> ELit (ct_value_to_lit v)

(** 尝试求值表达式为编译时常量 *)
let rec eval_at_compile_time expr =
  match expr with
  | ELit lit -> (match lit with
      | LInt n -> Some (CVInt n)
      | LFloat f -> Some (CVFloat f)
      | LBool b -> Some (CVBool b)
      | LString s -> Some (CVString s)
      | LChar c -> Some (CVChar c)
      | LUnit -> Some CVUnit)
  
  | EUnary (Neg, e) ->
      (match eval_at_compile_time e with
       | Some (CVInt n) -> Some (CVInt (-n))
       | Some (CVFloat f) -> Some (CVFloat (-.f))
       | _ -> None)
  
  | EUnary (Not, e) ->
      (match eval_at_compile_time e with
       | Some (CVBool b) -> Some (CVBool (not b))
       | _ -> None)
  
  | EBinary (op, e1, e2) ->
      (match eval_at_compile_time e1, eval_at_compile_time e2 with
       | Some v1, Some v2 -> eval_binary_op op v1 v2
       | _ -> None)
  
  | EList es ->
      let vs = List.filter_map eval_at_compile_time es in
      if List.length vs = List.length es then Some (CVList vs) else None
  
  | ETuple es ->
      let vs = List.filter_map eval_at_compile_time es in
      if List.length vs = List.length es then Some (CVTuple vs) else None
  
  | _ -> None

(** 求值二元运算符 *)
and eval_binary_op op v1 v2 =
  match op, v1, v2 with
  | Add, CVInt a, CVInt b -> Some (CVInt (a + b))
  | Add, CVFloat a, CVFloat b -> Some (CVFloat (a +. b))
  | Add, CVString a, CVString b -> Some (CVString (a ^ b))
  | Sub, CVInt a, CVInt b -> Some (CVInt (a - b))
  | Sub, CVFloat a, CVFloat b -> Some (CVFloat (a -. b))
  | Mul, CVInt a, CVInt b -> Some (CVInt (a * b))
  | Mul, CVFloat a, CVFloat b -> Some (CVFloat (a *. b))
  | Div, CVInt a, CVInt b -> if b <> 0 then Some (CVInt (a / b)) else None
  | Div, CVFloat a, CVFloat b -> Some (CVFloat (a /. b))
  | Mod, CVInt a, CVInt b -> if b <> 0 then Some (CVInt (a mod b)) else None
  | Eq, CVInt a, CVInt b -> Some (CVBool (a = b))
  | Eq, CVFloat a, CVFloat b -> Some (CVBool (a = b))
  | Eq, CVBool a, CVBool b -> Some (CVBool (a = b))
  | Eq, CVString a, CVString b -> Some (CVBool (a = b))
  | Eq, CVChar a, CVChar b -> Some (CVBool (a = b))
  | Neq, CVInt a, CVInt b -> Some (CVBool (a <> b))
  | Neq, CVFloat a, CVFloat b -> Some (CVBool (a <> b))
  | Neq, CVBool a, CVBool b -> Some (CVBool (a <> b))
  | Neq, CVString a, CVString b -> Some (CVBool (a <> b))
  | Lt, CVInt a, CVInt b -> Some (CVBool (a < b))
  | Lt, CVFloat a, CVFloat b -> Some (CVBool (a < b))
  | Le, CVInt a, CVInt b -> Some (CVBool (a <= b))
  | Le, CVFloat a, CVFloat b -> Some (CVBool (a <= b))
  | Gt, CVInt a, CVInt b -> Some (CVBool (a > b))
  | Gt, CVFloat a, CVFloat b -> Some (CVBool (a > b))
  | Ge, CVInt a, CVInt b -> Some (CVBool (a >= b))
  | Ge, CVFloat a, CVFloat b -> Some (CVBool (a >= b))
  | And, CVBool a, CVBool b -> Some (CVBool (a && b))
  | Or, CVBool a, CVBool b -> Some (CVBool (a || b))
  | Cons, v, CVList vs -> Some (CVList (v :: vs))
  | Concat, CVString a, CVString b -> Some (CVString (a ^ b))
  | _ -> None

(** 常量折叠：递归求值所有可求值的子表达式 *)
let rec constant_fold expr =
  match eval_at_compile_time expr with
  | Some v -> ct_value_to_expr v
  | None -> (match expr with
      | EBinary (op, e1, e2) ->
          let folded1 = constant_fold e1 in
          let folded2 = constant_fold e2 in
          (match eval_at_compile_time (EBinary (op, folded1, folded2)) with
           | Some v -> ct_value_to_expr v
           | None -> EBinary (op, folded1, folded2))
      | EUnary (op, e) ->
          let folded = constant_fold e in
          (match eval_at_compile_time (EUnary (op, folded)) with
           | Some v -> ct_value_to_expr v
           | None -> EUnary (op, folded))
      | ELet (x, v, body) -> ELet (x, constant_fold v, constant_fold body)
      | ELetRec (x, v, body) -> ELetRec (x, constant_fold v, constant_fold body)
      | EAssign (e1, e2) -> EAssign (constant_fold e1, constant_fold e2)
      | EIf (c, t, f) -> EIf (constant_fold c, constant_fold t, constant_fold f)
      | EMatch (e, cases) -> EMatch (constant_fold e, List.map (fun (p, body) -> (p, constant_fold body)) cases)
      | EWhile (c, b) -> EWhile (constant_fold c, constant_fold b)
      | ESeq (e1, e2) -> ESeq (constant_fold e1, constant_fold e2)
      | EFun (params, body) -> EFun (params, constant_fold body)
      | EApp (f, args) -> EApp (constant_fold f, List.map constant_fold args)
      | EList es -> EList (List.map constant_fold es)
      | ETuple es -> ETuple (List.map constant_fold es)
      | ERecord fields -> ERecord (List.map (fun (k, e) -> (k, constant_fold e)) fields)
      | EArray es -> EArray (List.map constant_fold es)
      | ERef e -> ERef (constant_fold e)
      | EDeref e -> EDeref (constant_fold e)
      | ETry (e, cases) -> ETry (constant_fold e, List.map (fun (p, body) -> (p, constant_fold body)) cases)
      | ERaise e -> ERaise (constant_fold e)
      | EAnnot (e, t) -> EAnnot (constant_fold e, t)
      | EQuote e -> EQuote (constant_fold e)
      | EAntiQuote e -> EAntiQuote (constant_fold e)
      | EMacro (name, args) -> EMacro (name, List.map constant_fold args)
      | EModule (name, body) -> EModule (name, constant_fold body)
      | EDot (e, field) -> EDot (constant_fold e, field)
      | e -> e)
