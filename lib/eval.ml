(** 求值器 *)

open Ast

exception RuntimeError of string

let lookup env x =
  match List.assoc_opt x env with
  | Some v -> v
  | None -> raise (RuntimeError ("Unbound variable: " ^ x))

(** eval 返回 (值, 新环境) *)
let rec eval env expr =
  match expr with
  | EInt n -> (VInt n, env)
  | EBool b -> (VBool b, env)
  | EString s -> (VString s, env)
  | EList es ->
      let vs, env' = eval_list env es in
      (VList vs, env')
  | ETuple es ->
      let vs, env' = eval_list env es in
      (VTuple vs, env')
  | EVar x -> (lookup env x, env)
  
  | EAdd (e1, e2) ->
      let v1, _ = eval env e1 in
      let v2, _ = eval env e2 in
      (match v1, v2 with
       | VInt a, VInt b -> (VInt (a + b), env)
       | _, _ -> raise (RuntimeError "Type error: + requires integers"))
  
  | ESub (e1, e2) ->
      let v1, _ = eval env e1 in
      let v2, _ = eval env e2 in
      (match v1, v2 with
       | VInt a, VInt b -> (VInt (a - b), env)
       | _, _ -> raise (RuntimeError "Type error: - requires integers"))
  
  | EMul (e1, e2) ->
      let v1, _ = eval env e1 in
      let v2, _ = eval env e2 in
      (match v1, v2 with
       | VInt a, VInt b -> (VInt (a * b), env)
       | _, _ -> raise (RuntimeError "Type error: * requires integers"))
  
  | EDiv (e1, e2) ->
      let v1, _ = eval env e1 in
      let v2, _ = eval env e2 in
      (match v1, v2 with
       | VInt _, VInt 0 -> raise (RuntimeError "Division by zero")
       | VInt a, VInt b -> (VInt (a / b), env)
       | _, _ -> raise (RuntimeError "Type error: / requires integers"))
  
  | EEq (e1, e2) ->
      let v1, _ = eval env e1 in
      let v2, _ = eval env e2 in
      (match v1, v2 with
       | VInt a, VInt b -> (VBool (a = b), env)
       | VBool a, VBool b -> (VBool (a = b), env)
       | VString a, VString b -> (VBool (a = b), env)
       | VUnit, VUnit -> (VBool true, env)
       | _, _ -> (VBool false, env))
  
  | ENeq (e1, e2) ->
      let v1, _ = eval env e1 in
      let v2, _ = eval env e2 in
      (match v1, v2 with
       | VInt a, VInt b -> (VBool (a <> b), env)
       | VBool a, VBool b -> (VBool (a <> b), env)
       | VString a, VString b -> (VBool (a <> b), env)
       | VUnit, VUnit -> (VBool false, env)
       | _, _ -> (VBool true, env))
  
  | ELt (e1, e2) ->
      let v1, _ = eval env e1 in
      let v2, _ = eval env e2 in
      (match v1, v2 with
       | VInt a, VInt b -> (VBool (a < b), env)
       | VString a, VString b -> (VBool (a < b), env)
       | _, _ -> raise (RuntimeError "Type error: < requires integers or strings"))
  
  | ELe (e1, e2) ->
      let v1, _ = eval env e1 in
      let v2, _ = eval env e2 in
      (match v1, v2 with
       | VInt a, VInt b -> (VBool (a <= b), env)
       | VString a, VString b -> (VBool (a <= b), env)
       | _, _ -> raise (RuntimeError "Type error: <= requires integers or strings"))
  
  | EGt (e1, e2) ->
      let v1, _ = eval env e1 in
      let v2, _ = eval env e2 in
      (match v1, v2 with
       | VInt a, VInt b -> (VBool (a > b), env)
       | VString a, VString b -> (VBool (a > b), env)
       | _, _ -> raise (RuntimeError "Type error: > requires integers or strings"))
  
  | EGe (e1, e2) ->
      let v1, _ = eval env e1 in
      let v2, _ = eval env e2 in
      (match v1, v2 with
       | VInt a, VInt b -> (VBool (a >= b), env)
       | VString a, VString b -> (VBool (a >= b), env)
       | _, _ -> raise (RuntimeError "Type error: >= requires integers or strings"))
  
  | EAnd (e1, e2) ->
      let v1, _ = eval env e1 in
      (match v1 with
       | VBool true -> eval env e2
       | VBool false -> (VBool false, env)
       | _ -> raise (RuntimeError "Type error: && requires booleans"))
  
  | EOr (e1, e2) ->
      let v1, _ = eval env e1 in
      (match v1 with
       | VBool true -> (VBool true, env)
       | VBool false -> eval env e2
       | _ -> raise (RuntimeError "Type error: || requires booleans"))
  
  | ENot e ->
      let v, _ = eval env e in
      (match v with
       | VBool b -> (VBool (not b), env)
       | _ -> raise (RuntimeError "Type error: not requires boolean"))
  
  | EIf (cond, then_branch, else_branch) ->
      let v, _ = eval env cond in
      (match v with
       | VBool true -> eval env then_branch
       | VBool false -> eval env else_branch
       | _ -> raise (RuntimeError "Type error: if requires boolean condition"))
  
  | ELet (x, value_expr, body) ->
      let value, env' = eval env value_expr in
      eval ((x, value) :: env') body
  
  | ELetRec (f, value_expr, body) ->
      (match value_expr with
       | EFun (param, func_body) ->
           let rec env' = (f, VFun (Some f, param, func_body, env')) :: env in
           eval env' body
       | _ -> raise (RuntimeError "let rec requires a function"))
  
  | EFun (param, body) -> (VFun (None, param, body, env), env)
  
  | EApp (func, arg) ->
      let func_val, _ = eval env func in
      let arg_val, _ = eval env arg in
      (match func_val with
       | VFun (name_opt, param, body, closure_env) ->
           let extended_env = (param, arg_val) :: closure_env in
           let extended_env =
             match name_opt with
             | Some name -> (name, func_val) :: extended_env
             | None -> extended_env
           in
           eval extended_env body
       | VBuiltin (_, f) -> f env arg_val
       | _ -> raise (RuntimeError "Type error: application requires function"))
  
  | ECat (e1, e2) ->
      let v1, _ = eval env e1 in
      let v2, _ = eval env e2 in
      (match v1, v2 with
       | VString a, VString b -> (VString (a ^ b), env)
       | _, _ -> raise (RuntimeError "Type error: ^ requires strings"))
  
  | ECons (e1, e2) ->
      let v1, _ = eval env e1 in
      let v2, _ = eval env e2 in
      (match v2 with
       | VList vs -> (VList (v1 :: vs), env)
       | _ -> raise (RuntimeError "Type error: :: requires a list on the right"))
  
  | EMatch (e, cases) ->
      let v, _ = eval env e in
      eval_match env v cases

  | ESeq (e1, e2) ->
      let _, env' = eval env e1 in
      eval env' e2

and eval_list env es =
  match es with
  | [] -> ([], env)
  | e :: rest ->
      let v, env' = eval env e in
      let vs, env'' = eval_list env' rest in
      (v :: vs, env'')

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

let builtin_type_env =
  [ ( "head",
      Types.Forall
        ( [0],
          Types.TArrow (Types.TList (Types.TVar 0), Types.TVar 0) ) )
  ; ( "tail",
      Types.Forall
        ( [0],
          Types.TArrow (Types.TList (Types.TVar 0), Types.TList (Types.TVar 0)) ) )
  ; ( "length",
      Types.Forall
        ( [0],
          Types.TArrow (Types.TList (Types.TVar 0), Types.TInt) ) )
  ; ( "print",
      Types.Forall
        ( [0],
          Types.TArrow (Types.TVar 0, Types.TUnit) ) )
  ; ( "import",
      Types.Forall
        ( [0],
          Types.TArrow (Types.TString, Types.TUnit) ) )
  ]

let builtin_env =
  let import_func env v =
    match v with
    | VString filename ->
        let content =
          try Core.In_channel.read_all filename
          with Sys_error msg -> raise (RuntimeError ("Cannot import file: " ^ msg))
        in
        let lexbuf = Lexing.from_string content in
        let expr = Parser.prog Lexer.read lexbuf in
        let _, env' = eval env expr in
        (VUnit, env')
    | _ -> raise (RuntimeError "import: expected string filename")
  in
  [ ( "head",
      VBuiltin
        ( "head",
          fun env -> function
          | VList (h :: _) -> (h, env)
          | VList [] -> raise (RuntimeError "head: empty list")
          | _ -> raise (RuntimeError "head: expected list") ) )
  ; ( "tail",
      VBuiltin
        ( "tail",
          fun env -> function
          | VList (_ :: t) -> (VList t, env)
          | VList [] -> raise (RuntimeError "tail: empty list")
          | _ -> raise (RuntimeError "tail: expected list") ) )
  ; ( "length",
      VBuiltin
        ( "length",
          fun env -> function
          | VList l -> (VInt (List.length l), env)
          | VString s -> (VInt (String.length s), env)
          | _ -> raise (RuntimeError "length: expected list or string") ) )
  ; ( "print",
      VBuiltin
        ( "print",
          fun env v ->
            print_endline (string_of_value v);
            (VUnit, env) ) )
  ; ( "import",
      VBuiltin
        ( "import",
          import_func ) )
  ]

let run expr =
  let v, _ = eval builtin_env expr in
  v
