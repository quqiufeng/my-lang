(** AST 优化器

    实现常量折叠、死代码消除、内联优化、公共子表达式消除。
*)

open Ast

(** 检查表达式是否是纯的（没有副作用） *)
let rec is_pure = function
  | EInt _ | EBool _ | EChar _ | EString _ | EVar _ | EFun _ -> true
  | EList es | ETuple es | EArray es -> List.for_all is_pure es
  | EAdd (a, b) | ESub (a, b) | EMul (a, b) | EDiv (a, b)
  | EEq (a, b) | ENeq (a, b) | ELt (a, b) | ELe (a, b) | EGt (a, b) | EGe (a, b)
  | EAnd (a, b) | EOr (a, b) | ECat (a, b) | ECons (a, b)
  | EIndex (a, b) | EArrayGet (a, b) | ERange (a, b) ->
      is_pure a && is_pure b
  | ENot e | EAnnot (e, _) | EDeref e -> is_pure e
  | ERecord fields -> List.for_all (fun (_, e) -> is_pure e) fields
  | ERecordGet (e, _) | EDot (e, _) -> is_pure e
  | ERecordUpdate (e, fields) ->
      is_pure e && List.for_all (fun (_, f) -> is_pure f) fields
  | EMatch (e, cases) ->
      is_pure e && List.for_all (fun (_, body) -> is_pure body) cases
  | EApp (f, arg) ->
      (* 只有已知纯函数的调用才是纯的 *)
      is_pure f && is_pure arg
  | EIf (cond, t, f) -> is_pure cond && is_pure t && is_pure f
  | ELet (_, e1, e2) | ELetRec (_, e1, e2) -> is_pure e1 && is_pure e2
  | ESeq (e1, e2) -> is_pure e1 && is_pure e2
  | EWhile (cond, body) -> is_pure cond && is_pure body
  | ERef e | ERaise e -> false  (* 有副作用 *)
  | EAssign _ -> false  (* 有副作用 *)
  | ETry (e, cases) -> is_pure e && List.for_all (fun (_, body) -> is_pure body) cases
  | ECtor (_, Some e) -> is_pure e
  | ECtor (_, None) -> true
  | ETypeDef _ | ETraitDef _ | ETraitImpl _ | EEffectDef _ -> true  (* 类型定义是纯的 *)
  | EModule (_, e) | EModuleType (_, e) -> is_pure e
  | EOpen _ -> true
  | ESlice (e, start, stop) ->
      is_pure e && (match start with None -> true | Some s -> is_pure s) && (match stop with None -> true | Some s -> is_pure s)
  | ESpawn _ | ESend _ | EReceive | EPerform _ | EHandle _ -> false  (* 并发/效果有副作用 *)

(** 检查变量是否在表达式中自由出现 *)
let rec appears_free var expr =
  match expr with
  | EVar x -> x = var
  | EInt _ | EBool _ | EChar _ | EString _ | EFun _ -> false
  | EList es | ETuple es | EArray es -> List.exists (appears_free var) es
  | EAdd (a, b) | ESub (a, b) | EMul (a, b) | EDiv (a, b)
  | EEq (a, b) | ENeq (a, b) | ELt (a, b) | ELe (a, b) | EGt (a, b) | EGe (a, b)
  | EAnd (a, b) | EOr (a, b) | ECat (a, b) | ECons (a, b)
  | EIndex (a, b) | EArrayGet (a, b) | ERange (a, b) | ESeq (a, b) | EAssign (a, b) | ESend (a, b) ->
      appears_free var a || appears_free var b
  | ENot e | EAnnot (e, _) | EDeref e | ERaise e | ERef e | ESpawn e | EPerform (_, e) ->
      appears_free var e
  | ERecord fields -> List.exists (fun (_, e) -> appears_free var e) fields
  | ERecordGet (e, _) | EDot (e, _) -> appears_free var e
  | ERecordUpdate (e, fields) ->
      appears_free var e || List.exists (fun (_, f) -> appears_free var f) fields
  | EMatch (e, cases) ->
      appears_free var e || List.exists (fun (_, body) -> appears_free var body) cases
  | EApp (f, arg) -> appears_free var f || appears_free var arg
  | EIf (cond, t, f) -> appears_free var cond || appears_free var t || appears_free var f
  | ELet (x, e1, e2) ->
      appears_free var e1 || (x <> var && appears_free var e2)
  | ELetRec (x, e1, e2) ->
      (x <> var && appears_free var e1) || (x <> var && appears_free var e2)
  | EWhile (cond, body) -> appears_free var cond || appears_free var body
  | ETry (e, cases) ->
      appears_free var e || List.exists (fun (_, body) -> appears_free var body) cases
  | ECtor (_, Some e) -> appears_free var e
  | ECtor (_, None) | ETypeDef _ | ETraitDef _ | ETraitImpl _ | EEffectDef _ | EOpen _ | EReceive -> false
  | EModule (_, e) | EModuleType (_, e) -> appears_free var e
  | EHandle (e, handlers) ->
      appears_free var e || List.exists (fun (_, _, _, body) -> appears_free var body) handlers
  | ESlice (e, start, stop) ->
      appears_free var e || (match start with None -> false | Some s -> appears_free var s) || (match stop with None -> false | Some s -> appears_free var s)

(** Beta 减少：内联简单函数 *)
let beta_reduce expr =
  match expr with
  | EApp (EFun (param, body), arg) when is_pure arg ->
      (* 内联纯参数 *)
      let rec substitute e =
        match e with
        | EVar x when x = param -> arg
        | EVar _ | EInt _ | EBool _ | EChar _ | EString _ | EFun _ -> e
        | EList es -> EList (List.map substitute es)
        | ETuple es -> ETuple (List.map substitute es)
        | EArray es -> EArray (List.map substitute es)
        | EAdd (a, b) -> EAdd (substitute a, substitute b)
        | ESub (a, b) -> ESub (substitute a, substitute b)
        | EMul (a, b) -> EMul (substitute a, substitute b)
        | EDiv (a, b) -> EDiv (substitute a, substitute b)
        | EEq (a, b) -> EEq (substitute a, substitute b)
        | ENeq (a, b) -> ENeq (substitute a, substitute b)
        | ELt (a, b) -> ELt (substitute a, substitute b)
        | ELe (a, b) -> ELe (substitute a, substitute b)
        | EGt (a, b) -> EGt (substitute a, substitute b)
        | EGe (a, b) -> EGe (substitute a, substitute b)
        | EAnd (a, b) -> EAnd (substitute a, substitute b)
        | EOr (a, b) -> EOr (substitute a, substitute b)
        | ECat (a, b) -> ECat (substitute a, substitute b)
        | ECons (a, b) -> ECons (substitute a, substitute b)
        | ENot e -> ENot (substitute e)
        | EAnnot (e, ty) -> EAnnot (substitute e, ty)
        | EIf (cond, t, f) -> EIf (substitute cond, substitute t, substitute f)
        | ELet (x, e1, e2) when x <> param -> ELet (x, substitute e1, substitute e2)
        | ELetRec (x, e1, e2) when x <> param -> ELetRec (x, substitute e1, substitute e2)
        | ESeq (a, b) -> ESeq (substitute a, substitute b)
        | EWhile (cond, body) -> EWhile (substitute cond, substitute body)
        | EIndex (a, b) -> EIndex (substitute a, substitute b)
        | ESlice (e, start, stop) -> ESlice (substitute e, Option.map substitute start, Option.map substitute stop)
        | EArrayGet (a, b) -> EArrayGet (substitute a, substitute b)
        | ERange (a, b) -> ERange (substitute a, substitute b)
        | ERecord fields -> ERecord (List.map (fun (k, v) -> (k, substitute v)) fields)
        | ERecordGet (e, field) -> ERecordGet (substitute e, field)
        | ERecordUpdate (e, fields) -> ERecordUpdate (substitute e, List.map (fun (k, v) -> (k, substitute v)) fields)
        | ERef e -> ERef (substitute e)
        | EDeref e -> EDeref (substitute e)
        | EAssign (a, b) -> EAssign (substitute a, substitute b)
        | ERaise e -> ERaise (substitute e)
        | EAnnot (e, ty) -> EAnnot (substitute e, ty)
        | ECtor (name, Some e) -> ECtor (name, Some (substitute e))
        | ECtor (name, None) -> ECtor (name, None)
        | EApp (f, arg) -> EApp (substitute f, substitute arg)
        | EMatch (e, cases) -> EMatch (substitute e, List.map (fun (p, b) -> (p, substitute b)) cases)
        | ESpawn e -> ESpawn (substitute e)
        | ESend (a, b) -> ESend (substitute a, substitute b)
        | EPerform (op, e) -> EPerform (op, substitute e)
        | EHandle (e, handlers) -> EHandle (substitute e, List.map (fun (op, arg, k, b) -> (op, arg, k, substitute b)) handlers)
        | EModule (name, e) -> EModule (name, substitute e)
        | EModuleType (name, e) -> EModuleType (name, substitute e)
        | _ -> e
      in
      substitute body
  | _ -> expr

(** 死代码消除 *)
let eliminate_dead_code expr =
  match expr with
  | ELet (_, e1, e2) when is_pure e1 ->
      (* 如果 e1 是纯的且结果未使用，可以消除 *)
      if appears_free "_" e2 then e2
      else expr
  | EIf (EBool true, then_branch, _) -> then_branch
  | EIf (EBool false, _, else_branch) -> else_branch
  | EAnd (EBool false, _) -> EBool false
  | EAnd (EBool true, e) -> e
  | EOr (EBool true, _) -> EBool true
  | EOr (EBool false, e) -> e
  | ESeq (e1, e2) when is_pure e1 -> e2
  | _ -> expr

(** 常量折叠 *)
let rec fold_constants expr =
  match expr with
  (* 算术运算 *)
  | EAdd (EInt a, EInt b) -> EInt (a + b)
  | ESub (EInt a, EInt b) -> EInt (a - b)
  | EMul (EInt a, EInt b) -> EInt (a * b)
  | EDiv (EInt a, EInt b) when b <> 0 -> EInt (a / b)
  | EDiv (EInt a, EInt b) when b = 0 -> expr  (* 保留除零错误 *)

  (* 比较运算 *)
  | EEq (EInt a, EInt b) -> EBool (a = b)
  | EEq (EBool a, EBool b) -> EBool (a = b)
  | EEq (EString a, EString b) -> EBool (String.equal a b)
  | ENeq (EInt a, EInt b) -> EBool (a <> b)
  | ENeq (EBool a, EBool b) -> EBool (a <> b)
  | ELt (EInt a, EInt b) -> EBool (a < b)
  | ELe (EInt a, EInt b) -> EBool (a <= b)
  | EGt (EInt a, EInt b) -> EBool (a > b)
  | EGe (EInt a, EInt b) -> EBool (a >= b)

  (* 逻辑运算 *)
  | EAnd (EBool a, EBool b) -> EBool (a && b)
  | EOr (EBool a, EBool b) -> EBool (a || b)
  | ENot (EBool a) -> EBool (not a)

  (* 短路求优：常量折叠特殊情况 *)
  | EAnd (EBool false, _) -> EBool false
  | EAnd (_, EBool false) when is_pure (match expr with EAnd (_, r) -> r | _ -> expr) -> EBool false
  | EAnd (EBool true, e) -> fold_constants e
  | EAnd (e, EBool true) -> fold_constants e
  | EOr (EBool true, _) -> EBool true
  | EOr (_, EBool true) when is_pure (match expr with EOr (_, r) -> r | _ -> expr) -> EBool true
  | EOr (EBool false, e) -> fold_constants e
  | EOr (e, EBool false) -> fold_constants e

  (* 字符串连接 *)
  | ECat (EString a, EString b) -> EString (a ^ b)

  (* 列表构造 *)
  | ECons (EInt a, EList bs) when List.for_all (function EInt _ -> true | _ -> false) bs ->
      EList (EInt a :: bs)

  (* 默认：递归处理子表达式 *)
  | EAdd (a, b) -> let a' = fold_constants a in let b' = fold_constants b in
      if a' <> a || b' <> b then fold_constants (EAdd (a', b')) else expr
  | ESub (a, b) -> let a' = fold_constants a in let b' = fold_constants b in
      if a' <> a || b' <> b then fold_constants (ESub (a', b')) else expr
  | EMul (a, b) -> let a' = fold_constants a in let b' = fold_constants b in
      if a' <> a || b' <> b then fold_constants (EMul (a', b')) else expr
  | EDiv (a, b) -> let a' = fold_constants a in let b' = fold_constants b in
      if a' <> a || b' <> b then fold_constants (EDiv (a', b')) else expr
  | EEq (a, b) -> let a' = fold_constants a in let b' = fold_constants b in
      if a' <> a || b' <> b then fold_constants (EEq (a', b')) else expr
  | ENeq (a, b) -> let a' = fold_constants a in let b' = fold_constants b in
      if a' <> a || b' <> b then fold_constants (ENeq (a', b')) else expr
  | ELt (a, b) -> let a' = fold_constants a in let b' = fold_constants b in
      if a' <> a || b' <> b then fold_constants (ELt (a', b')) else expr
  | ELe (a, b) -> let a' = fold_constants a in let b' = fold_constants b in
      if a' <> a || b' <> b then fold_constants (ELe (a', b')) else expr
  | EGt (a, b) -> let a' = fold_constants a in let b' = fold_constants b in
      if a' <> a || b' <> b then fold_constants (EGt (a', b')) else expr
  | EGe (a, b) -> let a' = fold_constants a in let b' = fold_constants b in
      if a' <> a || b' <> b then fold_constants (EGe (a', b')) else expr
  | EAnd (a, b) -> let a' = fold_constants a in let b' = fold_constants b in
      if a' <> a || b' <> b then fold_constants (EAnd (a', b')) else expr
  | EOr (a, b) -> let a' = fold_constants a in let b' = fold_constants b in
      if a' <> a || b' <> b then fold_constants (EOr (a', b')) else expr
  | ENot e -> let e' = fold_constants e in
      if e' <> e then fold_constants (ENot e') else expr
  | ECat (a, b) -> let a' = fold_constants a in let b' = fold_constants b in
      if a' <> a || b' <> b then fold_constants (ECat (a', b')) else expr

  (* 控制流 *)
  | EIf (cond, t, f) ->
      let cond' = fold_constants cond in
      (match cond' with
       | EBool true -> fold_constants t
       | EBool false -> fold_constants f
       | _ ->
           let t' = fold_constants t in
           let f' = fold_constants f in
           if cond' <> cond || t' <> t || f' <> f then
             fold_constants (EIf (cond', t', f'))
           else expr)

  | EWhile (cond, body) ->
      let cond' = fold_constants cond in
      (match cond' with
       | EBool false -> ETuple []  (* while false do ... done = () *)
       | _ ->
           let body' = fold_constants body in
           if cond' <> cond || body' <> body then EWhile (cond', body') else expr)

  | ESeq (e1, e2) ->
      let e1' = fold_constants e1 in
      let e2' = fold_constants e2 in
      (* 如果 e1 是纯的且结果不用，可以删除 *)
      if is_pure e1' && e1' <> e1 then
        fold_constants (ESeq (e1', e2'))
      else if e1' <> e1 || e2' <> e2 then
        ESeq (e1', e2')
      else expr

  | ELet (x, e1, e2) ->
      let e1' = fold_constants e1 in
      let e2' = fold_constants e2 in
      if e1' <> e1 || e2' <> e2 then ELet (x, e1', e2') else expr

  | ELetRec (x, e1, e2) ->
      let e1' = fold_constants e1 in
      let e2' = fold_constants e2 in
      if e1' <> e1 || e2' <> e2 then ELetRec (x, e1', e2') else expr

  | EFun (x, body) ->
      let body' = fold_constants body in
      if body' <> body then EFun (x, body') else expr

  | EApp (f, arg) ->
      let f' = fold_constants f in
      let arg' = fold_constants arg in
      if f' <> f || arg' <> arg then EApp (f', arg') else expr

  | EList es ->
      let es' = List.map fold_constants es in
      if es' <> es then EList es' else expr

  | ETuple es ->
      let es' = List.map fold_constants es in
      if es' <> es then ETuple es' else expr

  | EArray es ->
      let es' = List.map fold_constants es in
      if es' <> es then EArray es' else expr

  | EMatch (e, cases) ->
      let e' = fold_constants e in
      let cases' = List.map (fun (pat, body) -> (pat, fold_constants body)) cases in
      if e' <> e || cases' <> cases then EMatch (e', cases') else expr

  | ETry (e, cases) ->
      let e' = fold_constants e in
      let cases' = List.map (fun (pat, body) -> (pat, fold_constants body)) cases in
      if e' <> e || cases' <> cases then ETry (e', cases') else expr

  | ERecord fields ->
      let fields' = List.map (fun (k, e) -> (k, fold_constants e)) fields in
      if fields' <> fields then ERecord fields' else expr

  | ERecordGet (e, field) ->
      let e' = fold_constants e in
      if e' <> e then ERecordGet (e', field) else expr

  | ERecordUpdate (e, fields) ->
      let e' = fold_constants e in
      let fields' = List.map (fun (k, f) -> (k, fold_constants f)) fields in
      if e' <> e || fields' <> fields then ERecordUpdate (e', fields') else expr

  | EDot (e, field) ->
      let e' = fold_constants e in
      if e' <> e then EDot (e', field) else expr

  | EIndex (e, idx) ->
      let e' = fold_constants e in
      let idx' = fold_constants idx in
      if e' <> e || idx' <> idx then EIndex (e', idx') else expr

  | ESlice (e, start, stop) ->
      let e' = fold_constants e in
      let start' = Option.map fold_constants start in
      let stop' = Option.map fold_constants stop in
      if e' <> e || start' <> start || stop' <> stop then ESlice (e', start', stop') else expr

  | EArrayGet (e, idx) ->
      let e' = fold_constants e in
      let idx' = fold_constants idx in
      if e' <> e || idx' <> idx then EArrayGet (e', idx') else expr

  | ERange (e1, e2) ->
      let e1' = fold_constants e1 in
      let e2' = fold_constants e2 in
      if e1' <> e1 || e2' <> e2 then ERange (e1', e2') else expr

  | ERef e ->
      let e' = fold_constants e in
      if e' <> e then ERef e' else expr

  | EDeref e ->
      let e' = fold_constants e in
      if e' <> e then EDeref e' else expr

  | EAssign (e1, e2) ->
      let e1' = fold_constants e1 in
      let e2' = fold_constants e2 in
      if e1' <> e1 || e2' <> e2 then EAssign (e1', e2') else expr

  | ERaise e ->
      let e' = fold_constants e in
      if e' <> e then ERaise e' else expr

  | EAnnot (e, ty) ->
      let e' = fold_constants e in
      if e' <> e then EAnnot (e', ty) else expr

  | ECtor (name, Some e) ->
      let e' = fold_constants e in
      if e' <> e then ECtor (name, Some e') else expr

  | EModule (name, e) ->
      let e' = fold_constants e in
      if e' <> e then EModule (name, e') else expr

  | EModuleType (name, e) ->
      let e' = fold_constants e in
      if e' <> e then EModuleType (name, e') else expr

  | EHandle (e, handlers) ->
      let e' = fold_constants e in
      let handlers' = List.map (fun (op, arg, k, body) -> (op, arg, k, fold_constants body)) handlers in
      if e' <> e || handlers' <> handlers then EHandle (e', handlers') else expr

  | EPerform (op, e) ->
      let e' = fold_constants e in
      if e' <> e then EPerform (op, e') else expr

  | ESpawn e ->
      let e' = fold_constants e in
      if e' <> e then ESpawn e' else expr

  | ESend (e1, e2) ->
      let e1' = fold_constants e1 in
      let e2' = fold_constants e2 in
      if e1' <> e1 || e2' <> e2 then ESend (e1', e2') else expr

  | EReceive -> expr

  | ETypeDef _ | ETraitDef _ | ETraitImpl _ | EEffectDef _ | EOpen _ -> expr

  | ECons (a, b) ->
      let a' = fold_constants a in
      let b' = fold_constants b in
      if a' <> a || b' <> b then ECons (a', b') else expr

  | EInt _ | EBool _ | EChar _ | EString _ | EVar _ | ECtor (_, None) -> expr

(** 运行优化直到不动点 *)
let optimize expr =
  let rec loop e =
    let e' = fold_constants e in
    let e'' = beta_reduce e' in
    let e''' = eliminate_dead_code e'' in
    if e''' = e then e else loop e'''
  in
  loop expr

(** 优化表达式列表 *)
let optimize_list exprs = List.map optimize exprs
