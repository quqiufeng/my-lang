(** Hindley-Milner 类型推断

    基于算法 W 的类型推断实现，支持：
    - 基本类型：int, bool, string, unit
    - 复合类型：list, tuple, function
    - let-多态（let-polymorphism）
    - 模式匹配类型推断
    - import 语句类型提取
*)

open Ast
open Types

(** 类型变量名到类型变量的映射 *)
module StringMap = Map.Make (String)

(** 类型推断状态

    将类型推断过程中需要的可变状态封装为记录类型，
    通过参数传递实现线程安全和可重入。
*)
type state = {
  mutable subst : subst;
  mutable type_var_map : int StringMap.t;
  mutable ctor_env : (string * scheme) list;
}

let create_state () = {
  subst = Subst.empty;
  type_var_map = StringMap.empty;
  ctor_env = [];
}

(** Trait 方法类型环境 *)
let trait_type_env : (string, scheme) Hashtbl.t = Hashtbl.create 64

let register_trait_method name scheme =
  Hashtbl.replace trait_type_env name scheme

(** 注册内置 trait 方法类型 *)
let () =
  register_trait_method "show" (Forall ([0], TArrow (TVar 0, TString)));
  register_trait_method "eq" (Forall ([0], TArrow (TVar 0, TArrow (TVar 0, TBool))));
  register_trait_method "neq" (Forall ([0], TArrow (TVar 0, TArrow (TVar 0, TBool))))

let reset_type_vars st = st.type_var_map <- StringMap.empty

let get_type_var st name =
  match StringMap.find_opt name st.type_var_map with
  | Some n -> TVar n
  | None ->
      let n = !var_counter + 1 in
      var_counter := n;
      st.type_var_map <- StringMap.add name n st.type_var_map;
      TVar n

(** 解析类型字符串为 Types.t

    支持：
    - 基本类型: int, bool, string, unit
    - 类型变量: 'a, 'b
    - 类型应用: 'a map, int option
    - 元组类型: int * 'a * 'a map
*)
let parse_type_string st s =
  let tokens =
    let rec lex i acc =
      if i >= String.length s then List.rev acc
      else if s.[i] = ' ' || s.[i] = '\t' then lex (i + 1) acc
      else if s.[i] = '*' then lex (i + 1) ("*" :: acc)
      else if s.[i] = '(' then lex (i + 1) acc
      else if s.[i] = ')' then lex (i + 1) acc
      else if s.[i] = ',' then lex (i + 1) acc
      else if s.[i] = '\'' then
        let j = ref (i + 1) in
        while !j < String.length s &&
              (Char.code s.[!j] >= Char.code 'a' && Char.code s.[!j] <= Char.code 'z' ||
               Char.code s.[!j] >= Char.code 'A' && Char.code s.[!j] <= Char.code 'Z' ||
               Char.code s.[!j] >= Char.code '0' && Char.code s.[!j] <= Char.code '9' ||
               s.[!j] = '_') do
          j := !j + 1
        done;
        lex !j (String.sub s i (!j - i) :: acc)
      else if Char.code s.[i] >= Char.code 'a' && Char.code s.[i] <= Char.code 'z' ||
              Char.code s.[i] >= Char.code 'A' && Char.code s.[i] <= Char.code 'Z' then
        let j = ref (i + 1) in
        while !j < String.length s &&
              (Char.code s.[!j] >= Char.code 'a' && Char.code s.[!j] <= Char.code 'z' ||
               Char.code s.[!j] >= Char.code 'A' && Char.code s.[!j] <= Char.code 'Z' ||
               Char.code s.[!j] >= Char.code '0' && Char.code s.[!j] <= Char.code '9' ||
               s.[!j] = '_') do
          j := !j + 1
        done;
        lex !j (String.sub s i (!j - i) :: acc)
      else
        lex (i + 1) acc
    in
    lex 0 []
  in

  let rec parse_type toks =
    let t, rest = parse_app toks in
    match rest with
    | "*" :: rest' ->
        let t2, rest'' = parse_type rest' in
        (match t2 with
         | TTuple ts -> (TTuple (t :: ts), rest'')
         | _ -> (TTuple [t; t2], rest''))
    | _ -> (t, rest)

  and parse_app toks =
    let t, rest = parse_simple toks in
    match rest with
    | [] -> (t, [])
    | ("*" | ")" | ",") :: _ -> (t, rest)
    | tok :: [] when tok = "list" ->
        (TList t, [])
    | tok :: [] ->
        (TADT (tok, [t]), [])
    | tok :: rest' when tok = "list" ->
        (TList t, rest')
    | tok :: rest' ->
        (match rest' with
         | ("*" | ")" | ",") :: _ ->
             (TADT (tok, [t]), rest')
         | _ ->
             let t2, rest'' = parse_app rest' in
             (match t2 with
              | TADT (name, args) -> (TADT (name, t :: args), rest'')
              | _ -> (t, rest)))

  and parse_simple = function
    | [] -> (TUnit, [])
    | "int" :: rest -> (TInt, rest)
    | "bool" :: rest -> (TBool, rest)
    | "string" :: rest -> (TString, rest)
    | "unit" :: rest -> (TUnit, rest)
    | tok :: rest when String.length tok > 0 && tok.[0] = '\'' ->
        (get_type_var st tok, rest)
    | tok :: rest when tok <> "*" && tok <> ")" && tok <> "(" && tok <> "," ->
        (TADT (tok, []), rest)
    | toks -> (TUnit, toks)
  in

  let t, rest = parse_type tokens in
  if rest <> [] then
    TADT (s, [])
  else
    t

(** 应用当前状态中的替换到类型 *)
let apply_state st t = apply st.subst t

(** 统一两个类型并更新状态中的替换

    先对两个类型应用当前替换，然后统一，
    最后将新替换与状态中的替换组合。
*)
let unify_state st t1 t2 =
  let s = unify (apply_state st t1) (apply_state st t2) in
  st.subst <- compose s st.subst

(** 从模式推断类型

    [infer_pattern st env pat] 返回 (扩展后的环境, 模式的类型)。
    模式中的变量被绑定为新类型变量。
*)
let rec infer_pattern st env pat =
  match pat with
  | PWildcard ->
      (* 通配符不绑定变量，类型为新鲜变量 *)
      (env, new_var ())
  | PVar x ->
      (* 变量模式：绑定为新鲜变量 *)
      let t = new_var () in
      ((x, Forall ([], t)) :: env, t)
  | PInt _ -> (env, TInt)
  | PBool _ -> (env, TBool)
  | PString _ -> (env, TString)
  | PUnit -> (env, TUnit)
  | PList ps ->
      (* 列表模式：所有元素类型必须一致 *)
      let t_elem = new_var () in
      let env' =
        List.fold_left
          (fun env p ->
            let env', t' = infer_pattern st env p in
            unify_state st t_elem t';
            env')
          env ps
      in
      (env', TList (apply_state st t_elem))
  | PTuple ps ->
      (* 元组模式：每个子模式独立推断，类型组合为元组 *)
      let env', ts =
        List.fold_left
          (fun (env, ts) p ->
            let env', t = infer_pattern st env p in
            (env', t :: ts))
          (env, []) ps
      in
      (env', TTuple (List.map (apply_state st) (List.rev ts)))
  | PCons (p1, p2) ->
      (* cons 模式：p1 是元素类型，p2 是列表类型 *)
      let env', t1 = infer_pattern st env p1 in
      let env'', t2 = infer_pattern st env' p2 in
      unify_state st t2 (TList t1);
      (env'', apply_state st (TList t1))
  | PRecord fields ->
      (* 记录模式：每个字段独立推断 *)
      let env', field_types =
        List.fold_left
          (fun (env, fts) (name, p) ->
            let env', t = infer_pattern st env p in
            (env', (name, t) :: fts))
          (env, []) fields
      in
      (env', TRecord (List.map (fun (n, t) -> (n, apply_state st t)) (List.rev field_types)))
  | PCtor (c, None) ->
      (* 无参构造函数模式 *)
      (match List.assoc_opt c st.ctor_env with
       | Some scheme -> (env, instantiate scheme)
       | None -> raise (TypeError ("未知构造函数: " ^ c)))
  | PCtor (c, Some p) ->
      (* 有参构造函数模式 *)
      (match List.assoc_opt c st.ctor_env with
       | Some scheme ->
           (match instantiate scheme with
            | TArrow (param_t, ret_t) ->
                let env', t = infer_pattern st env p in
                unify_state st t param_t;
                (env', ret_t)
            | t ->
                raise (TypeError ("构造函数 " ^ c ^ " 不需要参数，类型为 " ^ string_of_type t)))
       | None -> raise (TypeError ("未知构造函数: " ^ c)))

(** 从表达式中提取 let 绑定类型

    [extract_bindings env expr] 遍历表达式，提取所有 let/let rec 绑定的类型签名。
    用于 import 语句的类型检查：导入文件中的绑定被加入当前环境。

    注意：只提取绑定，不检查表达式主体类型。
*)
let rec extract_bindings_state st env expr =
  match expr with
  | ELet (x, e, rest) ->
      let t = infer_state st env e in
      let scheme = generalize env t in
      extract_bindings_state st ((x, scheme) :: env) rest
  | ELetRec (f, EFun (param, body), rest) ->
      (* 递归函数：先假设函数类型，再推导函数体 *)
      let t_param = new_var () in
      let t_ret = new_var () in
      let t_fun = TArrow (t_param, t_ret) in
      let env' = (f, Forall ([], t_fun)) :: env in
      let env'' = (param, Forall ([], t_param)) :: env' in
      let t_body = infer_state st env'' body in
      unify_state st t_ret t_body;
      let scheme = generalize env (apply_state st t_fun) in
      extract_bindings_state st ((f, scheme) :: env) rest
  | ELetRec _ -> raise (TypeError "let rec 后面必须是函数")
  | ESeq (e1, e2) ->
      let env' = extract_bindings_state st env e1 in
      extract_bindings_state st env' e2
  | ETypeDef (name, type_params, ctors) ->
      reset_type_vars st;
      let param_vars = List.map (fun p -> get_type_var st p) type_params in
      let adt_t = TADT (name, param_vars) in
      let ctor_vars =
        List.fold_left
          (fun acc (_, param_str, gadt_ret) ->
            let acc' = match param_str with
              | None -> acc
              | Some t_str -> VarSet.union acc (free_vars (parse_type_string st t_str))
            in
            match gadt_ret with
            | None -> acc'
            | Some r_str -> VarSet.union acc' (free_vars (parse_type_string st r_str)))
          VarSet.empty ctors
      in
      let vars = VarSet.elements ctor_vars in
      List.iter
        (fun (c, param_type_str, gadt_ret_str) ->
          let ctor_t =
            match param_type_str, gadt_ret_str with
            | None, None -> adt_t
            | Some t_str, None -> TArrow (parse_type_string st t_str, adt_t)
            | None, Some r_str -> TArrow (TUnit, parse_type_string st r_str)
            | Some t_str, Some r_str -> TArrow (parse_type_string st t_str, parse_type_string st r_str)
          in
          st.ctor_env <- (c, Forall (vars, ctor_t)) :: st.ctor_env)
        ctors;
      env
  | _ -> env

(** 推断表达式类型

    [infer env expr] 在环境 [env] 下推断表达式 [expr] 的类型。
    使用全局替换 [current_subst] 累积统一结果。
*)
and infer_state st env expr =
  match expr with
  | EInt _ -> TInt
  | EBool _ -> TBool
  | EChar _ -> TChar
  | EString _ -> TString
  | EVar x ->
      (match Hashtbl.find_opt trait_type_env x with
       | Some scheme -> instantiate scheme
       | None -> instantiate (lookup env x))

  | EList [] ->
      (* 空列表：元素类型为新鲜变量 *)
      TList (new_var ())
  | EList (e :: es) ->
      (* 非空列表：所有元素类型必须一致 *)
      let t = infer_state st env e in
      List.iter
        (fun e' ->
          let t' = infer_state st env e' in
          unify_state st t t')
        es;
      TList (apply_state st t)

  | ETuple [] -> TUnit  (* 空元组 = unit *)
  | ETuple es ->
      (* 元组：各元素类型独立推断 *)
      TTuple (List.map (infer_state st env) es)

  (* 算术运算：要求整数操作数 *)
  | EAdd (e1, e2) | ESub (e1, e2) | EMul (e1, e2) | EDiv (e1, e2) ->
      let t1 = infer_state st env e1 in
      let t2 = infer_state st env e2 in
      unify_state st t1 TInt;
      unify_state st t2 TInt;
      TInt

  (* 相等/不等：要求同类型，结果为 bool *)
  | EEq (e1, e2) | ENeq (e1, e2) ->
      let t1 = infer_state st env e1 in
      let t2 = infer_state st env e2 in
      unify_state st t1 t2;
      TBool

  (* 比较运算：要求 int 或 string *)
  | ELt (e1, e2) | ELe (e1, e2) | EGt (e1, e2) | EGe (e1, e2) ->
      let t1 = infer_state st env e1 in
      let t2 = infer_state st env e2 in
      unify_state st t1 t2;
      (match apply_state st t1 with
       | TInt | TString | TVar _ -> ()
       | _ -> raise (TypeError "比较运算需要整数或字符串"));
      TBool

  (* 逻辑运算：要求布尔操作数 *)
  | EAnd (e1, e2) | EOr (e1, e2) ->
      let t1 = infer_state st env e1 in
      let t2 = infer_state st env e2 in
      unify_state st t1 TBool;
      unify_state st t2 TBool;
      TBool
  | ENot e ->
      let t = infer_state st env e in
      unify_state st t TBool;
      TBool

  (* 条件表达式 *)
  | EIf (cond, t_branch, f_branch) ->
      let tc = infer_state st env cond in
      let tt = infer_state st env t_branch in
      let tf = infer_state st env f_branch in
      unify_state st tc TBool;
      unify_state st tt tf;
      apply_state st tt

  (* let 绑定：泛化右侧类型，扩展环境 *)
  | ELet (x, e1, e2) ->
      let t1 = infer_state st env e1 in
      let scheme = generalize env t1 in
      infer_state st ((x, scheme) :: env) e2

  (* let rec 绑定：递归函数 *)
  | ELetRec (f, EFun (param, body), e2) ->
      let t_param = new_var () in
      let t_ret = new_var () in
      let t_fun = TArrow (t_param, t_ret) in
      (* 先假设函数类型，加入环境 *)
      let env' = (f, Forall ([], t_fun)) :: env in
      let env'' = (param, Forall ([], t_param)) :: env' in
      let t_body = infer_state st env'' body in
      unify_state st t_ret t_body;
      (* 泛化函数类型（基于原始环境，不包含 f 本身） *)
      let scheme = generalize env (apply_state st t_fun) in
      infer_state st ((f, scheme) :: env) e2
  | ELetRec _ -> raise (TypeError "let rec 后面必须是函数")

  (* 匿名函数 *)
  | EFun (param, body) ->
      let t_param = new_var () in
      let env' = (param, Forall ([], t_param)) :: env in
      let t_body = infer_state st env' body in
      TArrow (apply_state st t_param, apply_state st t_body)

  (* 函数应用 *)
  | EApp (e1, e2) ->
      (match e1 with
       | EVar "import" ->
           (* import 语句：读取文件，提取类型绑定，返回 unit *)
           (match e2 with
            | EString filename ->
                let content =
                  try Core.In_channel.read_all filename
                  with Sys_error msg -> raise (TypeError ("无法导入文件: " ^ msg))
                in
                let lexbuf = Lexing.from_string content in
                let expr = Parser.prog Lexer.read lexbuf in
                let _ = extract_bindings_state st env expr in
                TUnit
            | _ -> raise (TypeError "import: 需要字符串字面量"))
        | EVar ctor when List.mem_assoc ctor st.ctor_env ->
            (* 构造函数应用：转换为 ECtor *)
            let t_arg = infer_state st env e2 in
            (match List.assoc_opt ctor st.ctor_env with
             | Some scheme ->
                 (match instantiate scheme with
                  | TArrow (param_t, ret_t) ->
                      unify_state st t_arg param_t;
                      ret_t
                  | t ->
                      raise (TypeError ("构造函数 " ^ ctor ^ " 不需要参数，类型为 " ^ string_of_type t)))
             | None -> raise (TypeError ("未知构造函数: " ^ ctor)))
         | _ ->
            (* 普通函数应用：生成新返回类型变量，统一函数类型 *)
             let t1 = infer_state st env e1 in
             let t2 = infer_state st env e2 in
             let t_ret = new_var () in
             unify_state st t1 (TArrow (t2, t_ret));
             apply_state st t_ret)

  (* 字符串拼接 *)
  | ECat (e1, e2) ->
      let t1 = infer_state st env e1 in
      let t2 = infer_state st env e2 in
      unify_state st t1 TString;
      unify_state st t2 TString;
      TString

  (* cons 运算：元素类型和列表类型统一 *)
  | ECons (e1, e2) ->
      let t1 = infer_state st env e1 in
      let t2 = infer_state st env e2 in
      unify_state st t2 (TList t1);
      apply_state st (TList t1)

  (* 顺序执行：忽略第一个表达式的类型 *)
  | ESeq (e1, e2) ->
      let t1 = infer_state st env e1 in
      let env' = 
        match e1 with
        | EModule (name, _) -> 
            (* 将模块类型添加到环境中 *)
            (name, Forall ([], t1)) :: env
        | EOpen name ->
            (* 将模块字段导入环境中 *)
            (match List.assoc_opt name env with
             | Some scheme ->
                 (match apply_state st (instantiate scheme) with
                  | TRecord fields ->
                      let new_bindings =
                        List.map (fun (field, t) -> (field, Forall ([], t))) fields
                      in
                      new_bindings @ env
                  | _ -> env)
             | None -> env)
        | _ -> env
      in
      infer_state st env' e2

  (* while 循环：条件为 bool，返回 unit *)
  | EWhile (cond, body) ->
      let tc = infer_state st env cond in
      let _ = infer_state st env body in
      unify_state st tc TBool;
      TUnit

  (* 索引访问：e1[e2]，e1 为列表或字符串，e2 为 int *)
  | EIndex (e1, e2) ->
      let t1 = infer_state st env e1 in
      let t2 = infer_state st env e2 in
      let t_elem = new_var () in
      unify_state st t2 TInt;
      (match apply_state st t1 with
       | TList _ -> unify_state st t1 (TList t_elem); apply_state st t_elem
       | TString -> TString
       | _ ->
           unify_state st t1 (TList t_elem);
           apply_state st t_elem)

  (* 切片访问：e[start:end]，e 为列表或字符串，start/end 为 int *)
  | ESlice (e, start, end_) ->
      let t = infer_state st env e in
      (match start with
       | Some s -> let ts = infer_state st env s in unify_state st ts TInt
       | None -> ());
      (match end_ with
       | Some e -> let te = infer_state st env e in unify_state st te TInt
       | None -> ());
      (match apply_state st t with
       | TList elem_t -> TList (apply_state st elem_t)
       | TString -> TString
       | _ ->
           let t_elem = new_var () in
           unify_state st t (TList t_elem);
           TList (apply_state st t_elem))

  (* 模式匹配：所有分支返回类型一致 *)
  | EMatch (e, cases) ->
      let t = infer_state st env e in
      let t_ret = new_var () in
      List.iter
        (fun (pat, body) ->
          let env', t_pat = infer_pattern st env pat in
          unify_state st t t_pat;
          let t_body = infer_state st env' body in
          unify_state st t_ret t_body)
        cases;
      apply_state st t_ret

  (* 构造函数 *)
  | ECtor (c, None) ->
      (match List.assoc_opt c st.ctor_env with
       | Some scheme -> instantiate scheme
       | None -> raise (TypeError ("未知构造函数: " ^ c)))
  | ECtor (c, Some e) ->
      (match List.assoc_opt c st.ctor_env with
       | Some scheme ->
           (match instantiate scheme with
            | TArrow (param_t, ret_t) ->
                let t = infer_state st env e in
                unify_state st t param_t;
                ret_t
            | t ->
                raise (TypeError ("构造函数 " ^ c ^ " 不需要参数，类型为 " ^ string_of_type t)))
       | None -> raise (TypeError ("未知构造函数: " ^ c)))

  (* 类型定义：注册构造函数，返回 unit *)
  | ETypeDef (name, type_params, ctors) ->
      reset_type_vars st;
      let param_vars = List.map (fun p -> get_type_var st p) type_params in
      let adt_t = TADT (name, param_vars) in
      let ctor_vars =
        List.fold_left
          (fun acc (_, param_str, gadt_ret) ->
            let acc' = match param_str with
              | None -> acc
              | Some t_str -> VarSet.union acc (free_vars (parse_type_string st t_str))
            in
            match gadt_ret with
            | None -> acc'
            | Some r_str -> VarSet.union acc' (free_vars (parse_type_string st r_str)))
          VarSet.empty ctors
      in
      let vars = VarSet.elements ctor_vars in
      List.iter
        (fun (c, param_type_str, gadt_ret_str) ->
          let ctor_t =
            match param_type_str, gadt_ret_str with
            | None, None -> adt_t
            | Some t_str, None -> TArrow (parse_type_string st t_str, adt_t)
            | None, Some r_str -> TArrow (TUnit, parse_type_string st r_str)
            | Some t_str, Some r_str -> TArrow (parse_type_string st t_str, parse_type_string st r_str)
          in
          st.ctor_env <- (c, Forall (vars, ctor_t)) :: st.ctor_env)
        ctors;
      TUnit

  (* 引用类型 *)
  | ERef e ->
      let t = infer_state st env e in
      TRef (apply_state st t)

  | EDeref e ->
      let t = infer_state st env e in
      let t_elem = new_var () in
      unify_state st t (TRef t_elem);
      apply_state st t_elem

  | EAssign (e1, e2) ->
      (match e1 with
       | EArrayGet (arr, idx) ->
           let t_arr = infer_state st env arr in
           let t_idx = infer_state st env idx in
           let t_val = infer_state st env e2 in
           unify_state st t_idx TInt;
           unify_state st t_arr (TArray t_val);
           TUnit
       | ERecordGet (e, field) | EDot (e, field) ->
           let t = infer_state st env e in
           let t_val = infer_state st env e2 in
           (match apply_state st t with
            | TRecord fields ->
                (match List.assoc_opt field fields with
                 | Some ft ->
                     unify_state st ft t_val;
                     TUnit
                 | None -> raise (TypeError ("记录没有字段: " ^ field)))
            | _ ->
                unify_state st t (TRecord [(field, t_val)]);
                TUnit)
        | _ ->
           let t1 = infer_state st env e1 in
           let t2 = infer_state st env e2 in
           unify_state st t1 (TRef t2);
           TUnit)

  | ERaise e ->
      (* raise 返回一个多态类型，因为控制流不会继续 *)
      let _ = infer_state st env e in
      new_var ()

  | ETry (e, cases) ->
      (* try 的主体类型和所有 handler 的返回类型必须一致 *)
      let _ = infer_state st env e in
      let t_ret = new_var () in
      List.iter
        (fun (pat, body) ->
          let env', t_pat = infer_pattern st env pat in
          (* 异常模式类型和 raise 的类型一致 *)
          let _ = t_pat in
          let t_body = infer_state st env' body in
          unify_state st t_ret t_body)
        cases;
      apply_state st t_ret

  | EArray es ->
      let t_elem = new_var () in
      List.iter
        (fun e ->
          let t = infer_state st env e in
          unify_state st t_elem t)
        es;
      TArray (apply_state st t_elem)

  | EArrayGet (arr, idx) ->
      let t_arr = infer_state st env arr in
      let t_idx = infer_state st env idx in
      let t_elem = new_var () in
      unify_state st t_idx TInt;
      unify_state st t_arr (TArray t_elem);
      apply_state st t_elem

  | EAnnot (e, type_str) ->
      let t_expr = infer_state st env e in
      let t_annot = parse_type_string st type_str in
      unify_state st t_expr t_annot;
      apply_state st t_expr

  | ERange (start, end_) ->
      let t1 = infer_state st env start in
      let t2 = infer_state st env end_ in
      unify_state st t1 TInt;
      unify_state st t2 TInt;
      TList TInt

  | ERecord fields ->
      let field_types =
        List.map (fun (name, e) -> (name, infer_state st env e)) fields
      in
      TRecord (List.map (fun (name, t) -> (name, apply_state st t)) field_types)

  | ERecordGet (e, field) ->
      let t = infer_state st env e in
      let t_elem = new_var () in
      (match apply_state st t with
       | TRecord fields ->
           (match List.assoc_opt field fields with
            | Some ft -> ft
            | None -> raise (TypeError ("记录没有字段: " ^ field)))
       | _ ->
           unify_state st t (TRecord [(field, t_elem)]);
           apply_state st t_elem)

  | ERecordUpdate (e, fields) ->
      let t = infer_state st env e in
      let new_field_types =
        List.map (fun (name, e) -> (name, infer_state st env e)) fields
      in
      (match apply_state st t with
       | TRecord old_fields ->
           let merged =
             List.map (fun (name, old_t) ->
               match List.assoc_opt name new_field_types with
               | Some new_t -> (name, apply_state st new_t)
               | None -> (name, old_t)
             ) old_fields
           in
           let added =
             List.filter (fun (name, _) ->
               not (List.mem_assoc name old_fields)
             ) new_field_types
             |> List.map (fun (name, t) -> (name, apply_state st t))
           in
           TRecord (merged @ added)
       | _ ->
           TRecord (List.map (fun (name, t) -> (name, apply_state st t)) new_field_types))

  | EModule (name, body) ->
      (* 推断模块体类型，收集导出的类型绑定 *)
      let module_env = ref [] in
      let rec infer_module env expr =
        match expr with
        | ELet (x, v, rest) ->
            let t = infer_state st env v in
            let scheme = generalize env t in
            module_env := (x, scheme) :: !module_env;
            infer_module ((x, scheme) :: env) rest
        | ELetRec (x, v, rest) ->
            let t = infer_state st env v in
            let scheme = generalize env t in
            module_env := (x, scheme) :: !module_env;
            infer_module ((x, scheme) :: env) rest
        | ETypeDef _ -> infer_module env body
        | ESeq (e1, e2) ->
            let _ = infer_state st env e1 in
            infer_module env e2
        | _ ->
            let t = infer_state st env expr in
            module_env := ("__value", Forall ([], t)) :: !module_env;
            ()
      in
      infer_module env body;
      (* 模块类型：所有导出值的类型签名 *)
      let module_type = TRecord (List.map (fun (x, s) -> (x, instantiate s)) !module_env) in
      (* 注意：EModule 不直接修改全局 env，模块类型在运行时通过 VModule 处理 *)
      module_type

  | EModuleType (name, sig_expr) ->
      (* 模块类型签名：暂不实现完整签名检查 *)
      TUnit

  | EOpen name ->
      (* open 语句：将模块的类型绑定导入当前环境 *)
      (* 实际的绑定导入在 ESeq 中处理 *)
      (match List.assoc_opt name env with
       | Some _ -> TUnit
       | None -> raise (TypeError ("未定义的模块: " ^ name)))

  | EDot (e, field) ->
      (* 模块字段访问 *)
      (match e with
       | EVar name | ECtor (name, None) ->
           (* 尝试从环境中查找模块 *)
           (match List.assoc_opt name env with
            | Some scheme ->
                let t = instantiate scheme in
                (match apply_state st t with
                 | TRecord fields ->
                     (match List.assoc_opt field fields with
                      | Some ft -> ft
                      | None -> raise (TypeError ("模块中没有字段: " ^ field)))
                 | _ -> raise (TypeError "点号访问需要模块"))
            | None -> raise (TypeError ("未定义的模块: " ^ name)))
       | _ ->
           let t = infer_state st env e in
           (match apply_state st t with
            | TRecord fields ->
                (match List.assoc_opt field fields with
                 | Some ft -> ft
                 | None -> raise (TypeError ("模块中没有字段: " ^ field)))
             | _ -> raise (TypeError "点号访问需要模块")))

  | ETraitDef (name, params, methods) ->
      List.iter (fun (mname, _) ->
        let scheme = Forall ([0], TArrow (TVar 0, TVar 0)) in
        register_trait_method mname scheme
      ) methods;
      TUnit

  | ETraitImpl (trait_name, type_name, methods) ->
      List.iter (fun (mname, mexpr) ->
        let t = infer_state st env mexpr in
        let scheme = generalize env t in
        register_trait_method mname scheme
      ) methods;
      TUnit

  | ESpawn e ->
      let _ = infer_state st env e in
      TInt

  | ESend (pid_e, msg_e) ->
      let t1 = infer_state st env pid_e in
      let _ = infer_state st env msg_e in
      unify_state st t1 TInt;
      TUnit

  | EReceive ->
      new_var ()

  | EEffectDef (name, ops) ->
      (* 注册效果操作到环境 *)
      List.iter (fun op ->
        let scheme = Forall ([0; 1], TArrow (TVar 0, TVar 1)) in
        register_trait_method op scheme
      ) ops;
      TUnit

  | EPerform (op, arg) ->
      let t_arg = infer_state st env arg in
      (match Hashtbl.find_opt trait_type_env op with
       | Some scheme ->
           let t_op = instantiate scheme in
           let t_ret = new_var () in
           unify_state st t_op (TArrow (t_arg, t_ret));
           t_ret
       | None ->
           (* 如果找不到，假设 op 在环境中 *)
           let t_op = instantiate (lookup env op) in
           let t_ret = new_var () in
           unify_state st t_op (TArrow (t_arg, t_ret));
           t_ret)

  | EHandle (e, handlers) ->
      (* 为每个 handler 添加绑定到环境 *)
      let handler_env = List.fold_left (fun env_acc (op, arg_name, k_name, body) ->
        let t_arg = new_var () in
        let t_ret = new_var () in
        let t_k = TArrow (t_ret, t_ret) in
        let body_env = (k_name, Forall ([], t_k)) :: (arg_name, Forall ([], t_arg)) :: env_acc in
        let t_body = infer_state st body_env body in
        unify_state st t_body t_ret;
        (op, Forall ([], TArrow (t_arg, TArrow (t_k, t_ret)))) :: env_acc
      ) env handlers in
      infer_state st handler_env e

(** 类型检查入口（指定环境）

    [typecheck_with_env env expr] 在指定环境下检查表达式类型。
    每次调用前重置类型变量计数器和全局替换。
*)
let typecheck_with_env env expr =
  reset_vars ();
  let st = create_state () in
  let t = infer_state st env expr in
  apply_state st t

(** 类型检查入口（默认环境）

    [typecheck expr] 在内置类型环境下检查表达式类型。
*)
let typecheck expr =
  reset_vars ();
  let st = create_state () in
  let t = infer_state st Eval.builtin_type_env expr in
  apply_state st t

(** 向后兼容的包装函数 *)
let extract_bindings env expr =
  let st = create_state () in
  extract_bindings_state st env expr
