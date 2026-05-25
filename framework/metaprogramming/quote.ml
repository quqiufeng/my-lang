(** Quote/Anti-quote 实现

    基于通用 AST 类型实现 quote 和 anti-quote。
*)

open Ast_types

(** Quote 节点：将 AST 包装为值 *)
let quote_expr e = EQuote e

(** Anti-quote 节点：在 quote 中插入外部值 *)
let anti_quote_expr e = EAntiQuote e

(** 检查表达式是否在 quote 上下文中 *)
let rec in_quote_context = function
  | EQuote _ -> true
  | EBinary (_, e1, e2) -> in_quote_context e1 || in_quote_context e2
  | EUnary (_, e) -> in_quote_context e
  | ELet (_, v, body) -> in_quote_context v || in_quote_context body
  | ELetRec (_, v, body) -> in_quote_context v || in_quote_context body
  | EAssign (e1, e2) -> in_quote_context e1 || in_quote_context e2
  | EIf (c, t, f) -> in_quote_context c || in_quote_context t || in_quote_context f
  | EMatch (e, cases) -> in_quote_context e || List.exists (fun (_, body) -> in_quote_context body) cases
  | EWhile (c, b) -> in_quote_context c || in_quote_context b
  | ESeq (e1, e2) -> in_quote_context e1 || in_quote_context e2
  | EFun (_, body) -> in_quote_context body
  | EApp (f, args) -> in_quote_context f || List.exists in_quote_context args
  | EList es -> List.exists in_quote_context es
  | ETuple es -> List.exists in_quote_context es
  | ERecord fields -> List.exists (fun (_, e) -> in_quote_context e) fields
  | EArray es -> List.exists in_quote_context es
  | ERef e -> in_quote_context e
  | EDeref e -> in_quote_context e
  | ETry (e, cases) -> in_quote_context e || List.exists (fun (_, body) -> in_quote_context body) cases
  | ERaise e -> in_quote_context e
  | EAnnot (e, _) -> in_quote_context e
  | EAntiQuote _ -> true
  | EMacro (_, args) -> List.exists in_quote_context args
  | EDot (e, _) -> in_quote_context e
  | _ -> false

(** 展开 quote 和 anti-quote

    quote e -> 将 e 转为表示其 AST 的表达式
    ~e 在 quote 中 -> 将 e 的值嵌入到生成的 AST 中
*)
let rec expand_quotes expr =
  match expr with
  | EQuote e -> ast_to_expr e  (* 将 AST 转为构造该 AST 的表达式 *)
  | EAntiQuote e -> e  (* anti-quote 返回外部表达式的值 *)
  | EBinary (op, e1, e2) -> EBinary (op, expand_quotes e1, expand_quotes e2)
  | EUnary (op, e) -> EUnary (op, expand_quotes e)
  | ELet (x, v, body) -> ELet (x, expand_quotes v, expand_quotes body)
  | ELetRec (x, v, body) -> ELetRec (x, expand_quotes v, expand_quotes body)
  | EAssign (e1, e2) -> EAssign (expand_quotes e1, expand_quotes e2)
  | EIf (c, t, f) -> EIf (expand_quotes c, expand_quotes t, expand_quotes f)
  | EMatch (e, cases) -> EMatch (expand_quotes e, List.map (fun (p, body) -> (p, expand_quotes body)) cases)
  | EWhile (c, b) -> EWhile (expand_quotes c, expand_quotes b)
  | ESeq (e1, e2) -> ESeq (expand_quotes e1, expand_quotes e2)
  | EFun (params, body) -> EFun (params, expand_quotes body)
  | EApp (f, args) -> EApp (expand_quotes f, List.map expand_quotes args)
  | EList es -> EList (List.map expand_quotes es)
  | ETuple es -> ETuple (List.map expand_quotes es)
  | ERecord fields -> ERecord (List.map (fun (k, e) -> (k, expand_quotes e)) fields)
  | EArray es -> EArray (List.map expand_quotes es)
  | ERef e -> ERef (expand_quotes e)
  | EDeref e -> EDeref (expand_quotes e)
  | ETry (e, cases) -> ETry (expand_quotes e, List.map (fun (p, body) -> (p, expand_quotes body)) cases)
  | ERaise e -> ERaise (expand_quotes e)
  | EAnnot (e, t) -> EAnnot (expand_quotes e, t)
  | EMacro (name, args) -> EMacro (name, List.map expand_quotes args)
  | EModule (name, body) -> EModule (name, expand_quotes body)
  | EDot (e, field) -> EDot (expand_quotes e, field)
  | e -> e

(** 将 AST 转为构造该 AST 的表达式

    例如：EInt 42 -> EApp (EVar "ELit", [EApp (EVar "LInt", [ELit (LInt 42)])])
*)
and ast_to_expr = function
  | ELit lit ->
      let lit_expr = match lit with
        | LInt n -> EApp (EVar "LInt", [ELit (LInt n)])
        | LFloat f -> EApp (EVar "LFloat", [ELit (LFloat f)])
        | LBool b -> EApp (EVar "LBool", [ELit (LBool b)])
        | LString s -> EApp (EVar "LString", [ELit (LString s)])
        | LChar c -> EApp (EVar "LChar", [ELit (LChar c)])
        | LUnit -> EVar "LUnit"
      in
      EApp (EVar "ELit", [lit_expr])
  | EVar x -> EApp (EVar "EVar", [ELit (LString x)])
  | EBinary (op, e1, e2) ->
      let op_expr = match op with
        | Add -> EVar "Add" | Sub -> EVar "Sub" | Mul -> EVar "Mul"
        | Div -> EVar "Div" | Mod -> EVar "Mod"
        | Eq -> EVar "Eq" | Neq -> EVar "Neq"
        | Lt -> EVar "Lt" | Le -> EVar "Le" | Gt -> EVar "Gt" | Ge -> EVar "Ge"
        | And -> EVar "And" | Or -> EVar "Or"
        | Cons -> EVar "Cons" | Concat -> EVar "Concat"
      in
      EApp (EVar "EBinary", [op_expr; ast_to_expr e1; ast_to_expr e2])
  | EUnary (op, e) ->
      let op_expr = match op with
        | Neg -> EVar "Neg" | Not -> EVar "Not" | Deref -> EVar "Deref"
      in
      EApp (EVar "EUnary", [op_expr; ast_to_expr e])
  | ELet (x, v, body) -> EApp (EVar "ELet", [ELit (LString x); ast_to_expr v; ast_to_expr body])
  | EFun (params, body) ->
      EApp (EVar "EFun", [EList (List.map (fun p -> ELit (LString p)) params); ast_to_expr body])
  | EApp (f, args) ->
      EApp (EVar "EApp", [ast_to_expr f; EList (List.map ast_to_expr args)])
  | EList es -> EApp (EVar "EList", [EList (List.map ast_to_expr es)])
  | ETuple es -> EApp (EVar "ETuple", [EList (List.map ast_to_expr es)])
  | _ -> EVar "(* complex AST *)"

(** 简单的 AST 遍历工具 *)
let rec map f expr =
  let mapped = match expr with
    | EBinary (op, e1, e2) -> EBinary (op, map f e1, map f e2)
    | EUnary (op, e) -> EUnary (op, map f e)
    | ELet (x, v, body) -> ELet (x, map f v, map f body)
    | ELetRec (x, v, body) -> ELetRec (x, map f v, map f body)
    | EAssign (e1, e2) -> EAssign (map f e1, map f e2)
    | EIf (c, t, fl) -> EIf (map f c, map f t, map f fl)
    | EMatch (e, cases) -> EMatch (map f e, List.map (fun (p, body) -> (p, map f body)) cases)
    | EWhile (c, b) -> EWhile (map f c, map f b)
    | ESeq (e1, e2) -> ESeq (map f e1, map f e2)
    | EFun (params, body) -> EFun (params, map f body)
    | EApp (fn, args) -> EApp (map f fn, List.map (map f) args)
    | EList es -> EList (List.map (map f) es)
    | ETuple es -> ETuple (List.map (map f) es)
    | ERecord fields -> ERecord (List.map (fun (k, e) -> (k, map f e)) fields)
    | EArray es -> EArray (List.map (map f) es)
    | ERef e -> ERef (map f e)
    | EDeref e -> EDeref (map f e)
    | ETry (e, cases) -> ETry (map f e, List.map (fun (p, body) -> (p, map f body)) cases)
    | ERaise e -> ERaise (map f e)
    | EAnnot (e, t) -> EAnnot (map f e, t)
    | EQuote e -> EQuote (map f e)
    | EAntiQuote e -> EAntiQuote (map f e)
    | EMacro (name, args) -> EMacro (name, List.map (map f) args)
    | EModule (name, body) -> EModule (name, map f body)
    | EDot (e, field) -> EDot (map f e, field)
    | e -> e
  in
  f mapped

let rec fold f acc expr =
  let acc' = f acc expr in
  match expr with
  | EBinary (_, e1, e2) -> fold f (fold f acc' e1) e2
  | EUnary (_, e) -> fold f acc' e
  | ELet (_, v, body) -> fold f (fold f acc' v) body
  | ELetRec (_, v, body) -> fold f (fold f acc' v) body
  | EAssign (e1, e2) -> fold f (fold f acc' e1) e2
  | EIf (c, t, fl) -> fold f (fold f (fold f acc' c) t) fl
  | EMatch (e, cases) -> List.fold_left (fun acc (_, body) -> fold f acc body) (fold f acc' e) cases
  | EWhile (c, b) -> fold f (fold f acc' c) b
  | ESeq (e1, e2) -> fold f (fold f acc' e1) e2
  | EFun (_, body) -> fold f acc' body
  | EApp (fn, args) -> List.fold_left (fold f) (fold f acc' fn) args
  | EList es | ETuple es | EArray es -> List.fold_left (fold f) acc' es
  | ERecord fields -> List.fold_left (fun acc (_, e) -> fold f acc e) acc' fields
  | ERef e | EDeref e | ERaise e | EAnnot (e, _) | EQuote e | EAntiQuote e -> fold f acc' e
  | ETry (e, cases) -> List.fold_left (fun acc (_, body) -> fold f acc body) (fold f acc' e) cases
  | EMacro (_, args) -> List.fold_left (fold f) acc' args
  | EModule (_, body) -> fold f acc' body
  | EDot (e, _) -> fold f acc' e
  | _ -> acc'

let exists pred expr =
  fold (fun acc e -> acc || pred e) false expr
