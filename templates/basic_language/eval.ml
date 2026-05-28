(** 基础语言模板 - 求值器 *)

open Ast

(** 运行时异常 *)
exception RuntimeError of string

(** 求值表达式 *)
let rec eval env = function
  | EInt n -> VInt n
  | EBool b -> VBool b
  | EString s -> VString s
  | EUnit -> VUnit
  | EVar x ->
      (match List.assoc_opt x env with
       | Some v -> v
       | None -> raise (RuntimeError ("Unbound variable: " ^ x)))
  | ELet (x, e1, e2) ->
      let v1 = eval env e1 in
      eval ((x, v1) :: env) e2
  | EFun (params, body) ->
      VFun (params, body, env)
  | EApp (f, args) ->
      let fv = eval env f in
      let argvs = List.map (eval env) args in
      apply_value fv argvs
  | EIf (cond, then_, else_) ->
      (match eval env cond with
       | VBool true -> eval env then_
       | VBool false -> eval env else_
       | _ -> raise (RuntimeError "Condition must be boolean"))
  | EMatch (e, cases) ->
      let v = eval env e in
      eval_match env v cases
  | EBinary (op, e1, e2) ->
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      eval_binary_op op v1 v2
  | EUnary (op, e) ->
      let v = eval env e in
      eval_unary_op op v
  | ETuple es ->
      VTuple (List.map (eval env) es)
  | EList es ->
      VList (List.map (eval env) es)
  | ESeq (e1, e2) ->
      let _ = eval env e1 in
      eval env e2

(** 应用函数 *)
and apply_value f args =
  match f with
  | VFun (params, body, closure_env) ->
      if List.length params <> List.length args then
        raise (RuntimeError "Wrong number of arguments")
      else
        let env' = List.combine params args @ closure_env in
        eval env' body
  | VBuiltin (_, f) -> f args
  | _ -> raise (RuntimeError "Not a function")

(** 模式匹配 *)
and eval_match env v = function
  | [] -> raise (RuntimeError "Match failure")
  | (p, body) :: rest ->
      (match match_pattern p v with
       | Some bindings -> eval (bindings @ env) body
       | None -> eval_match env v rest)

(** 模式匹配 *)
and match_pattern p v =
  match p, v with
  | PVar x, _ -> Some [(x, v)]
  | PInt n, VInt m when n = m -> Some []
  | PBool b, VBool c when b = c -> Some []
  | PWild, _ -> Some []
  | PTuple ps, VTuple vs when List.length ps = List.length vs ->
      let rec match_all = function
        | [], [] -> Some []
        | p :: ps, v :: vs ->
            (match match_pattern p v with
             | Some b1 ->
                 (match match_all (ps, vs) with
                  | Some b2 -> Some (b1 @ b2)
                  | None -> None)
             | None -> None)
        | _ -> None
      in
      match_all (ps, vs)
  | PList ps, VList vs when List.length ps = List.length vs ->
      let rec match_all = function
        | [], [] -> Some []
        | p :: ps, v :: vs ->
            (match match_pattern p v with
             | Some b1 ->
                 (match match_all (ps, vs) with
                  | Some b2 -> Some (b1 @ b2)
                  | None -> None)
             | None -> None)
        | _ -> None
      in
      match_all (ps, vs)
  | _ -> None

(** 二元运算 *)
and eval_binary_op op v1 v2 =
  match op, v1, v2 with
  | Add, VInt a, VInt b -> VInt (a + b)
  | Sub, VInt a, VInt b -> VInt (a - b)
  | Mul, VInt a, VInt b -> VInt (a * b)
  | Div, VInt _, VInt 0 -> raise (RuntimeError "Division by zero")
  | Div, VInt a, VInt b -> VInt (a / b)
  | Eq, VInt a, VInt b -> VBool (a = b)
  | Eq, VBool a, VBool b -> VBool (a = b)
  | Eq, VString a, VString b -> VBool (a = b)
  | Neq, VInt a, VInt b -> VBool (a <> b)
  | Neq, VBool a, VBool b -> VBool (a <> b)
  | Lt, VInt a, VInt b -> VBool (a < b)
  | Le, VInt a, VInt b -> VBool (a <= b)
  | Gt, VInt a, VInt b -> VBool (a > b)
  | Ge, VInt a, VInt b -> VBool (a >= b)
  | And, VBool a, VBool b -> VBool (a && b)
  | Or, VBool a, VBool b -> VBool (a || b)
  | _ -> raise (RuntimeError "Type error in binary operation")

(** 一元运算 *)
and eval_unary_op op v =
  match op, v with
  | Neg, VInt n -> VInt (-n)
  | Not, VBool b -> VBool (not b)
  | _ -> raise (RuntimeError "Type error in unary operation")

(** 内置函数 *)
let builtin_env = [
  ("print", VBuiltin ("print", fun args ->
    List.iter (fun v -> print_endline (string_of_value v)) args;
    VUnit));
  ("length", VBuiltin ("length", fun args ->
    match args with
    | [VList l] -> VInt (List.length l)
    | [VString s] -> VInt (String.length s)
    | _ -> raise (RuntimeError "length expects a list or string")));
]

(** 运行程序 *)
let run source =
  try
    let lexbuf = Lexing.from_string source in
    let ast = Parser.prog Lexer.read lexbuf in
    let result = eval builtin_env ast in
    Ok result
  with
  | Lexer.SyntaxError msg -> Error ("Syntax error: " ^ msg)
  | Parser.Error -> Error "Parse error"
  | RuntimeError msg -> Error ("Runtime error: " ^ msg)
  | exn -> Error ("Error: " ^ Printexc.to_string exn)
