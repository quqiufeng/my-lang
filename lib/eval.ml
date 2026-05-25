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
       | VBuiltin (_, f) -> f arg_val
       | _ -> raise (RuntimeError "Type error: application requires function"))
  
  | ECat (e1, e2) ->
      (match eval env e1, eval env e2 with
       | VString a, VString b -> VString (a ^ b)
       | _, _ -> raise (RuntimeError "Type error: ^ requires strings"))
  
  | ECons (e1, e2) ->
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      (match v2 with
       | VList vs -> VList (v1 :: vs)
       | _ -> raise (RuntimeError "Type error: :: requires a list on the right"))
  
  | EMatch (e, cases) ->
      let v = eval env e in
      eval_match env v cases

  | ESeq (e1, e2) ->
      let _ = eval env e1 in
      eval env e2

and eval_match env v cases =
  match cases with
  | [] -> raise (RuntimeError "Match failure: no matching pattern")
  | (p, body) :: rest ->
      (match match_pattern p v with
       | Some bindings -> eval (bindings @ env) body
       | None -> eval_match env v rest)

and match_pattern pat value =
  match pat, value with
  | PWildcard, _ -> Some []
  | PVar x, v -> Some [(x, v)]
  | PInt n, VInt m when n = m -> Some []
  | PBool b, VBool c when b = c -> Some []
  | PString s, VString t when s = t -> Some []
  | PUnit, VUnit -> Some []
  | PList ps, VList vs when List.length ps = List.length vs ->
      match_patterns ps vs
  | PTuple ps, VTuple vs when List.length ps = List.length vs ->
      match_patterns ps vs
  | PCons (p1, p2), VList (h :: t) ->
      (match match_pattern p1 h with
       | Some b1 ->
           (match match_pattern p2 (VList t) with
            | Some b2 -> Some (b1 @ b2)
            | None -> None)
       | None -> None)
  | _ -> None

and match_patterns ps vs =
  match ps, vs with
  | [], [] -> Some []
  | p :: ps', v :: vs' ->
      (match match_pattern p v with
       | Some b1 ->
           (match match_patterns ps' vs' with
            | Some b2 -> Some (b1 @ b2)
            | None -> None)
       | None -> None)
  | _ -> None

let builtin_env =
  [ ( "head",
      VBuiltin
        ( "head",
          function
          | VList (h :: _) -> h
          | VList [] -> raise (RuntimeError "head: empty list")
          | _ -> raise (RuntimeError "head: expected list") ) )
  ; ( "tail",
      VBuiltin
        ( "tail",
          function
          | VList (_ :: t) -> VList t
          | VList [] -> raise (RuntimeError "tail: empty list")
          | _ -> raise (RuntimeError "tail: expected list") ) )
  ; ( "length",
      VBuiltin
        ( "length",
          function
          | VList l -> VInt (List.length l)
          | VString s -> VInt (String.length s)
          | _ -> raise (RuntimeError "length: expected list or string") ) )
  ; ( "print",
      VBuiltin
        ( "print",
          fun v ->
            print_endline (string_of_value v);
            VUnit ) )
  ]

let run expr = eval builtin_env expr
