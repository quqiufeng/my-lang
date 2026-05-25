(** 求值器 *)

open Ast

exception RuntimeError of string

let lookup env x =
  match List.assoc_opt x env with
  | Some v -> v
  | None -> raise (RuntimeError ("Unbound variable: " ^ x))

let rec eval env expr =
  match expr with
  | EInt n -> VInt n
  | EBool b -> VBool b
  | EVar x -> lookup env x
  
  | EAdd (e1, e2) ->
      (match eval env e1, eval env e2 with
       | VInt a, VInt b -> VInt (a + b)
       | _, _ -> raise (RuntimeError "Type error: + requires integers"))
  
  | ESub (e1, e2) ->
      (match eval env e1, eval env e2 with
       | VInt a, VInt b -> VInt (a - b)
       | _, _ -> raise (RuntimeError "Type error: - requires integers"))
  
  | EMul (e1, e2) ->
      (match eval env e1, eval env e2 with
       | VInt a, VInt b -> VInt (a * b)
       | _, _ -> raise (RuntimeError "Type error: * requires integers"))
  
  | EDiv (e1, e2) ->
      (match eval env e1, eval env e2 with
       | VInt _, VInt 0 -> raise (RuntimeError "Division by zero")
       | VInt a, VInt b -> VInt (a / b)
       | _, _ -> raise (RuntimeError "Type error: / requires integers"))
  
  | EEq (e1, e2) ->
      (match eval env e1, eval env e2 with
       | VInt a, VInt b -> VBool (a = b)
       | VBool a, VBool b -> VBool (a = b)
       | _, _ -> raise (RuntimeError "Type error: = requires same types"))
  
  | ENeq (e1, e2) ->
      (match eval env e1, eval env e2 with
       | VInt a, VInt b -> VBool (a <> b)
       | VBool a, VBool b -> VBool (a <> b)
       | _, _ -> raise (RuntimeError "Type error: <> requires same types"))
  
  | ELt (e1, e2) ->
      (match eval env e1, eval env e2 with
       | VInt a, VInt b -> VBool (a < b)
       | _, _ -> raise (RuntimeError "Type error: < requires integers"))
  
  | ELe (e1, e2) ->
      (match eval env e1, eval env e2 with
       | VInt a, VInt b -> VBool (a <= b)
       | _, _ -> raise (RuntimeError "Type error: <= requires integers"))
  
  | EGt (e1, e2) ->
      (match eval env e1, eval env e2 with
       | VInt a, VInt b -> VBool (a > b)
       | _, _ -> raise (RuntimeError "Type error: > requires integers"))
  
  | EGe (e1, e2) ->
      (match eval env e1, eval env e2 with
       | VInt a, VInt b -> VBool (a >= b)
       | _, _ -> raise (RuntimeError "Type error: >= requires integers"))
  
  | EAnd (e1, e2) ->
      (match eval env e1 with
       | VBool true -> eval env e2
       | VBool false -> VBool false
       | _ -> raise (RuntimeError "Type error: && requires booleans"))
  
  | EOr (e1, e2) ->
      (match eval env e1 with
       | VBool true -> VBool true
       | VBool false -> eval env e2
       | _ -> raise (RuntimeError "Type error: || requires booleans"))
  
  | ENot e ->
      (match eval env e with
       | VBool b -> VBool (not b)
       | _ -> raise (RuntimeError "Type error: not requires boolean"))
  
  | EIf (cond, then_branch, else_branch) ->
      (match eval env cond with
       | VBool true -> eval env then_branch
       | VBool false -> eval env else_branch
       | _ -> raise (RuntimeError "Type error: if requires boolean condition"))
  
  | ELet (x, value_expr, body) ->
      let value = eval env value_expr in
      eval ((x, value) :: env) body
  
  | EFun (param, body) ->
      VFun (param, body, env)
  
  | EApp (func, arg) ->
      let func_val = eval env func in
      let arg_val = eval env arg in
      (match func_val with
       | VFun (param, body, closure_env) ->
           eval ((param, arg_val) :: closure_env) body
       | _ -> raise (RuntimeError "Type error: application requires function"))

let run expr = eval [] expr
