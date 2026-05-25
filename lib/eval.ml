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
  | EString s -> VString s
  | EList es -> VList (List.map (eval env) es)
  | ETuple es -> VTuple (List.map (eval env) es)
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
       | VString a, VString b -> VBool (a = b)
       | VUnit, VUnit -> VBool true
       | _, _ -> VBool false)
  
  | ENeq (e1, e2) ->
      (match eval env e1, eval env e2 with
       | VInt a, VInt b -> VBool (a <> b)
       | VBool a, VBool b -> VBool (a <> b)
       | VString a, VString b -> VBool (a <> b)
       | VUnit, VUnit -> VBool false
       | _, _ -> VBool true)
  
  | ELt (e1, e2) ->
      (match eval env e1, eval env e2 with
       | VInt a, VInt b -> VBool (a < b)
       | VString a, VString b -> VBool (a < b)
       | _, _ -> raise (RuntimeError "Type error: < requires integers or strings"))
  
  | ELe (e1, e2) ->
      (match eval env e1, eval env e2 with
       | VInt a, VInt b -> VBool (a <= b)
       | VString a, VString b -> VBool (a <= b)
       | _, _ -> raise (RuntimeError "Type error: <= requires integers or strings"))
  
  | EGt (e1, e2) ->
      (match eval env e1, eval env e2 with
       | VInt a, VInt b -> VBool (a > b)
       | VString a, VString b -> VBool (a > b)
       | _, _ -> raise (RuntimeError "Type error: > requires integers or strings"))
  
  | EGe (e1, e2) ->
      (match eval env e1, eval env e2 with
       | VInt a, VInt b -> VBool (a >= b)
       | VString a, VString b -> VBool (a >= b)
       | _, _ -> raise (RuntimeError "Type error: >= requires integers or strings"))
  
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
  
  | ELetRec (f, value_expr, body) ->
      (match value_expr with
       | EFun (param, func_body) ->
           let rec env' = (f, VFun (Some f, param, func_body, env')) :: env in
           eval env' body
       | _ -> raise (RuntimeError "let rec requires a function"))
  
  | EFun (param, body) ->
      VFun (None, param, body, env)
  
  | EApp (func, arg) ->
      let func_val = eval env func in
      let arg_val = eval env arg in
      (match func_val with
       | VFun (name_opt, param, body, closure_env) ->
           let extended_env = (param, arg_val) :: closure_env in
           let extended_env =
             match name_opt with
             | Some name -> (name, func_val) :: extended_env
             | None -> extended_env
           in
           eval extended_env body
       | _ -> raise (RuntimeError "Type error: application requires function"))
  
  | ECons (e1, e2) ->
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      (match v2 with
       | VList vs -> VList (v1 :: vs)
       | _ -> raise (RuntimeError "Type error: :: requires a list on the right"))
  
  | ESeq (e1, e2) ->
      let _ = eval env e1 in
      eval env e2

let run expr = eval [] expr
