(** Hindley-Milner 类型推断 *)

open Ast
open Types

(** 全局替换表 *)
let current_subst = ref []

let apply_current t = apply !current_subst t

let unify_ref t1 t2 =
  let s = unify (apply_current t1) (apply_current t2) in
  current_subst := compose s !current_subst

(** 从模式推断类型，返回 (扩展环境, 模式类型) *)
let rec infer_pattern env pat =
  match pat with
  | PWildcard -> (env, new_var ())
  | PVar x ->
      let t = new_var () in
      ((x, Forall ([], t)) :: env, t)
  | PInt _ -> (env, TInt)
  | PBool _ -> (env, TBool)
  | PString _ -> (env, TString)
  | PUnit -> (env, TUnit)
  | PList ps ->
      let t_elem = new_var () in
      let env' =
        List.fold_left
          (fun env p ->
            let env', t' = infer_pattern env p in
            unify_ref t_elem t';
            env')
          env ps
      in
      (env', TList (apply_current t_elem))
  | PTuple ps ->
      let env', ts =
        List.fold_left
          (fun (env, ts) p ->
            let env', t = infer_pattern env p in
            (env', t :: ts))
          (env, []) ps
      in
      (env', TTuple (List.map apply_current (List.rev ts)))
  | PCons (p1, p2) ->
      let env', t1 = infer_pattern env p1 in
      let env'', t2 = infer_pattern env' p2 in
      unify_ref t2 (TList t1);
      (env'', apply_current (TList t1))

(** 从表达式中提取 let 绑定类型，用于 import *)
let rec extract_bindings env expr =
  match expr with
  | ELet (x, e, rest) ->
      let t = infer env e in
      let scheme = generalize env t in
      extract_bindings ((x, scheme) :: env) rest
  | ELetRec (f, EFun (param, body), rest) ->
      let t_param = new_var () in
      let t_ret = new_var () in
      let t_fun = TArrow (t_param, t_ret) in
      let env' = (f, Forall ([], t_fun)) :: env in
      let env'' = (param, Forall ([], t_param)) :: env' in
      let t_body = infer env'' body in
      unify_ref t_ret t_body;
      let scheme = generalize env (apply_current t_fun) in
      extract_bindings ((f, scheme) :: env) rest
  | ELetRec _ -> raise (TypeError "let rec requires a function")
  | ESeq (e1, e2) ->
      let env' = extract_bindings env e1 in
      extract_bindings env' e2
  | _ -> env

(** 推断表达式类型 *)
and infer env expr =
  match expr with
  | EInt _ -> TInt
  | EBool _ -> TBool
  | EString _ -> TString
  | EVar x -> instantiate (lookup env x)
  | EList [] -> TList (new_var ())
  | EList (e :: es) ->
      let t = infer env e in
      List.iter
        (fun e' ->
          let t' = infer env e' in
          unify_ref t t')
        es;
      TList (apply_current t)
  | ETuple es -> TTuple (List.map (infer env) es)
  | EAdd (e1, e2) | ESub (e1, e2) | EMul (e1, e2) | EDiv (e1, e2) ->
      let t1 = infer env e1 in
      let t2 = infer env e2 in
      unify_ref t1 TInt;
      unify_ref t2 TInt;
      TInt
  | EEq (e1, e2) | ENeq (e1, e2) ->
      let t1 = infer env e1 in
      let t2 = infer env e2 in
      unify_ref t1 t2;
      TBool
  | ELt (e1, e2) | ELe (e1, e2) | EGt (e1, e2) | EGe (e1, e2) ->
      let t1 = infer env e1 in
      let t2 = infer env e2 in
      unify_ref t1 t2;
      (match apply_current t1 with
       | TInt | TString -> ()
       | _ -> raise (TypeError "comparison requires int or string"));
      TBool
  | EAnd (e1, e2) | EOr (e1, e2) ->
      let t1 = infer env e1 in
      let t2 = infer env e2 in
      unify_ref t1 TBool;
      unify_ref t2 TBool;
      TBool
  | ENot e ->
      let t = infer env e in
      unify_ref t TBool;
      TBool
  | EIf (cond, t_branch, f_branch) ->
      let tc = infer env cond in
      let tt = infer env t_branch in
      let tf = infer env f_branch in
      unify_ref tc TBool;
      unify_ref tt tf;
      apply_current tt
  | ELet (x, e1, e2) ->
      let t1 = infer env e1 in
      let scheme = generalize env t1 in
      infer ((x, scheme) :: env) e2
  | ELetRec (f, EFun (param, body), e2) ->
      let t_param = new_var () in
      let t_ret = new_var () in
      let t_fun = TArrow (t_param, t_ret) in
      let env' = (f, Forall ([], t_fun)) :: env in
      let env'' = (param, Forall ([], t_param)) :: env' in
      let t_body = infer env'' body in
      unify_ref t_ret t_body;
      let scheme = generalize env (apply_current t_fun) in
      infer ((f, scheme) :: env) e2
  | ELetRec _ -> raise (TypeError "let rec requires a function")
  | EFun (param, body) ->
      let t_param = new_var () in
      let env' = (param, Forall ([], t_param)) :: env in
      let t_body = infer env' body in
      TArrow (apply_current t_param, apply_current t_body)
  | EApp (e1, e2) ->
      (match e1 with
       | EVar "import" ->
           (match e2 with
            | EString filename ->
                let content =
                  try Core.In_channel.read_all filename
                  with Sys_error msg -> raise (TypeError ("Cannot import file: " ^ msg))
                in
                let lexbuf = Lexing.from_string content in
                let expr = Parser.prog Lexer.read lexbuf in
                 let _ = extract_bindings env expr in
                 TUnit
            | _ -> raise (TypeError "import: expected string literal"))
       | _ ->
           let t1 = infer env e1 in
           let t2 = infer env e2 in
           let t_ret = new_var () in
           unify_ref t1 (TArrow (t2, t_ret));
           apply_current t_ret)
  | ECat (e1, e2) ->
      let t1 = infer env e1 in
      let t2 = infer env e2 in
      unify_ref t1 TString;
      unify_ref t2 TString;
      TString
  | ECons (e1, e2) ->
      let t1 = infer env e1 in
      let t2 = infer env e2 in
      unify_ref t2 (TList t1);
      apply_current (TList t1)
  | ESeq (e1, e2) ->
      let _ = infer env e1 in
      infer env e2
  | EMatch (e, cases) ->
      let t = infer env e in
      let t_ret = new_var () in
      List.iter
        (fun (pat, body) ->
          let env', t_pat = infer_pattern env pat in
          unify_ref t t_pat;
          let t_body = infer env' body in
          unify_ref t_ret t_body)
        cases;
      apply_current t_ret

(** 类型检查入口（指定环境） *)
let typecheck_with_env env expr =
  reset_vars ();
  current_subst := [];
  let t = infer env expr in
  apply_current t

(** 类型检查入口（默认环境） *)
let typecheck expr =
  reset_vars ();
  current_subst := [];
  let t = infer Eval.builtin_type_env expr in
  apply_current t
