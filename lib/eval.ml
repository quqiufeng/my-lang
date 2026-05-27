(** 求值器 *)

open Ast

(** 安全的列表索引，避免两次遍历 *)
let list_nth_safe lst idx =
  let rec loop i = function
    | [] -> None
    | x :: _ when i = idx -> Some x
    | _ :: xs -> loop (i + 1) xs
  in
  if idx < 0 then None else loop 0 lst

exception RuntimeError of string * pos option
exception Exception_value of value

(** Trait 方法表：key = "trait#method#type" -> value *)
let trait_method_table : (string, value) Hashtbl.t = Hashtbl.create 64

let make_trait_key trait_name method_name type_name =
  trait_name ^ "#" ^ method_name ^ "#" ^ type_name

(** 全局 trait 环境 *)
let trait_env = ref (Traits.builtin_traits ())

(** 获取值的类型描述（用于错误报告） *)
let rec type_of_value = function
  | VInt _ -> "int"
  | VBool _ -> "bool"
  | VChar _ -> "char"
  | VString _ -> "string"
  | VList _ -> "list"
  | VTuple _ -> "tuple"
  | VFun _ -> "function"
  | VBuiltin _ -> "builtin"
  | VUnit -> "unit"
  | VCtor (name, None) -> name
  | VCtor (name, Some _) -> name
  | VRef _ -> "ref"
  | VExn (name, _) -> "exception:" ^ name
  | VArray _ -> "array"
  | VRecord _ -> "record"
  | VModule _ -> "module"

(** 注册内置 trait 实现 *)
let () =
  Traits.add_default_impls !trait_env;
  (* 手动注册内置实现到 trait_method_table *)
  let int_show = VBuiltin ("show", fun env arg ->
    match arg with
    | VInt n -> (VString (string_of_int n), env)
    | v -> raise (RuntimeError ("show: 需要 int，但得到 " ^ type_of_value v, None))) in
  Hashtbl.replace trait_method_table (make_trait_key "Show" "show" "int") int_show;
  let bool_show = VBuiltin ("show", fun env arg ->
    match arg with
    | VBool b -> (VString (string_of_bool b), env)
    | v -> raise (RuntimeError ("show: 需要 bool，但得到 " ^ type_of_value v, None))) in
  Hashtbl.replace trait_method_table (make_trait_key "Show" "show" "bool") bool_show;
  let int_eq = VBuiltin ("eq", fun env arg ->
    (VBuiltin ("eq'", fun env arg2 ->
      match arg, arg2 with
      | VInt a, VInt b -> (VBool (a = b), env)
      | v1, v2 -> raise (RuntimeError ("eq: 需要两个 int，但得到 " ^ type_of_value v1 ^ " 和 " ^ type_of_value v2, None))),
     env)) in
  Hashtbl.replace trait_method_table (make_trait_key "Eq" "eq" "int") int_eq;
  let int_neq = VBuiltin ("neq", fun env arg ->
    (VBuiltin ("neq'", fun env arg2 ->
      match arg, arg2 with
      | VInt a, VInt b -> (VBool (a <> b), env)
      | v1, v2 -> raise (RuntimeError ("neq: 需要两个 int，但得到 " ^ type_of_value v1 ^ " 和 " ^ type_of_value v2, None))),
     env)) in
  Hashtbl.replace trait_method_table (make_trait_key "Eq" "neq" "int") int_neq

let lookup env x =
  match List.assoc_opt x env with
  | Some v -> v
  | None -> raise (RuntimeError ("未绑定变量: " ^ x, None))

(** 应用函数值到参数 *)
let rec apply_value env func arg =
  match func with
  | VFun (name_opt, param, body, closure_env) ->
      let extended_env = (param, arg) :: closure_env in
      let extended_env =
        match name_opt with
        | Some name -> (name, func) :: extended_env
        | None -> extended_env
      in
      eval extended_env body
  | VBuiltin (_, f) -> f env arg
  | v -> raise (RuntimeError ("应用需要函数，但得到 " ^ type_of_value v, None))

(** eval 返回 (值, 新环境) *)
and eval env expr =
  match expr with
  | EInt n -> (VInt n, env)
  | EBool b -> (VBool b, env)
  | EChar c -> (VChar c, env)
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
       | VInt _, v2 -> raise (RuntimeError ("类型错误: + 的右操作数是 " ^ type_of_value v2 ^ "，需要整数", None))
       | v1, _ -> raise (RuntimeError ("类型错误: + 的左操作数是 " ^ type_of_value v1 ^ "，需要整数", None)))
  
  | ESub (e1, e2) ->
      let v1, _ = eval env e1 in
      let v2, _ = eval env e2 in
      (match v1, v2 with
       | VInt a, VInt b -> (VInt (a - b), env)
       | VInt _, v2 -> raise (RuntimeError ("类型错误: - 的右操作数是 " ^ type_of_value v2 ^ "，需要整数", None))
       | v1, _ -> raise (RuntimeError ("类型错误: - 的左操作数是 " ^ type_of_value v1 ^ "，需要整数", None)))
  
  | EMul (e1, e2) ->
      let v1, _ = eval env e1 in
      let v2, _ = eval env e2 in
      (match v1, v2 with
       | VInt a, VInt b -> (VInt (a * b), env)
       | VInt _, v2 -> raise (RuntimeError ("类型错误: * 的右操作数是 " ^ type_of_value v2 ^ "，需要整数", None))
       | v1, _ -> raise (RuntimeError ("类型错误: * 的左操作数是 " ^ type_of_value v1 ^ "，需要整数", None)))
  
  | EDiv (e1, e2) ->
      let v1, _ = eval env e1 in
      let v2, _ = eval env e2 in
      (match v1, v2 with
       | VInt _, VInt 0 -> raise (RuntimeError ("除零错误", None))
       | VInt a, VInt b -> (VInt (a / b), env)
       | VInt _, v2 -> raise (RuntimeError ("类型错误: / 的右操作数是 " ^ type_of_value v2 ^ "，需要整数", None))
       | v1, _ -> raise (RuntimeError ("类型错误: / 的左操作数是 " ^ type_of_value v1 ^ "，需要整数", None)))
  
  | EEq (e1, e2) ->
      let v1, _ = eval env e1 in
      let v2, _ = eval env e2 in
       (match v1, v2 with
        | VInt a, VInt b -> (VBool (a = b), env)
        | VBool a, VBool b -> (VBool (a = b), env)
        | VString a, VString b -> (VBool (a = b), env)
        | VChar a, VChar b -> (VBool (Char.equal a b), env)
        | VUnit, VUnit -> (VBool true, env)
        | _, _ -> (VBool false, env))
  
  | ENeq (e1, e2) ->
      let v1, _ = eval env e1 in
      let v2, _ = eval env e2 in
       (match v1, v2 with
        | VInt a, VInt b -> (VBool (a <> b), env)
        | VBool a, VBool b -> (VBool (a <> b), env)
        | VString a, VString b -> (VBool (a <> b), env)
        | VChar a, VChar b -> (VBool (not (Char.equal a b)), env)
        | VUnit, VUnit -> (VBool false, env)
        | _, _ -> (VBool true, env))
  
  | ELt (e1, e2) ->
      let v1, _ = eval env e1 in
      let v2, _ = eval env e2 in
      (match v1, v2 with
       | VInt a, VInt b -> (VBool (a < b), env)
       | VString a, VString b -> (VBool (a < b), env)
       | v1, v2 -> raise (RuntimeError ("类型错误: < 的操作数是 " ^ type_of_value v1 ^ " 和 " ^ type_of_value v2 ^ "，需要整数或字符串", None)))
  
  | ELe (e1, e2) ->
      let v1, _ = eval env e1 in
      let v2, _ = eval env e2 in
      (match v1, v2 with
       | VInt a, VInt b -> (VBool (a <= b), env)
       | VString a, VString b -> (VBool (a <= b), env)
       | v1, v2 -> raise (RuntimeError ("类型错误: <= 的操作数是 " ^ type_of_value v1 ^ " 和 " ^ type_of_value v2 ^ "，需要整数或字符串", None)))
  
  | EGt (e1, e2) ->
      let v1, _ = eval env e1 in
      let v2, _ = eval env e2 in
      (match v1, v2 with
       | VInt a, VInt b -> (VBool (a > b), env)
       | VString a, VString b -> (VBool (a > b), env)
       | v1, v2 -> raise (RuntimeError ("类型错误: > 的操作数是 " ^ type_of_value v1 ^ " 和 " ^ type_of_value v2 ^ "，需要整数或字符串", None)))
  
  | EGe (e1, e2) ->
      let v1, _ = eval env e1 in
      let v2, _ = eval env e2 in
      (match v1, v2 with
       | VInt a, VInt b -> (VBool (a >= b), env)
       | VString a, VString b -> (VBool (a >= b), env)
       | v1, v2 -> raise (RuntimeError ("类型错误: >= 的操作数是 " ^ type_of_value v1 ^ " 和 " ^ type_of_value v2 ^ "，需要整数或字符串", None)))
  
  | EAnd (e1, e2) ->
      let v1, _ = eval env e1 in
      (match v1 with
       | VBool true -> eval env e2
       | VBool false -> (VBool false, env)
        | v -> raise (RuntimeError ("类型错误: && 的操作数是 " ^ type_of_value v ^ "，需要布尔值", None)))
  
  | EOr (e1, e2) ->
      let v1, _ = eval env e1 in
      (match v1 with
       | VBool true -> (VBool true, env)
       | VBool false -> eval env e2
       | v -> raise (RuntimeError ("类型错误: || 的操作数是 " ^ type_of_value v ^ "，需要布尔值", None)))
  
  | ENot e ->
      let v, _ = eval env e in
      (match v with
       | VBool b -> (VBool (not b), env)
       | v -> raise (RuntimeError ("类型错误: not 的操作数是 " ^ type_of_value v ^ "，需要布尔值", None)))
  
  | EIf (cond, then_branch, else_branch) ->
      let v, _ = eval env cond in
      (match v with
       | VBool true -> eval env then_branch
       | VBool false -> eval env else_branch
       | v -> raise (RuntimeError ("类型错误: if 的条件是 " ^ type_of_value v ^ "，需要布尔值", None)))
  
  | ELet (x, value_expr, body) ->
      let value, _ = eval env value_expr in
      eval ((x, value) :: env) body
  
  | ELetRec (f, value_expr, body) ->
      (match value_expr with
       | EFun (param, func_body) ->
           let rec env' = (f, VFun (Some f, param, func_body, env')) :: env in
           eval env' body
        | _ -> raise (RuntimeError ("let rec 后面必须是函数", None)))
  
  | EFun (param, body) -> (VFun (None, param, body, env), env)
  
  | EApp (func, arg) ->
      let func_val, _ = eval env func in
      let arg_val, _ = eval env arg in
      (match func_val with
       | VCtor (c, None) -> (VCtor (c, Some arg_val), env)
       | _ ->
         try
           let v, env' = apply_value env func_val arg_val in
           (v, env')
         with
            | RuntimeError (msg, _) -> raise (RuntimeError (msg, None)))
  
  | ECat (e1, e2) ->
      let v1, _ = eval env e1 in
      let v2, _ = eval env e2 in
      (match v1, v2 with
       | VString a, VString b -> (VString (a ^ b), env)
       | v1, v2 -> raise (RuntimeError ("类型错误: ^ 的操作数是 " ^ type_of_value v1 ^ " 和 " ^ type_of_value v2 ^ "，需要字符串", None)))
  
  | ECons (e1, e2) ->
      let v1, _ = eval env e1 in
      let v2, _ = eval env e2 in
      (match v2 with
       | VList vs -> (VList (v1 :: vs), env)
       | v -> raise (RuntimeError ("类型错误: :: 的右边是 " ^ type_of_value v ^ "，需要列表", None)))
  
  | EMatch (e, cases) ->
      let v, _ = eval env e in
      eval_match env v cases

  | ESeq (e1, e2) ->
      let _, env' = eval env e1 in
      eval env' e2

  | EWhile (cond, body) ->
      let rec loop env =
        let v, _ = eval env cond in
        match v with
        | VBool true ->
            let _, env' = eval env body in
            loop env'
        | VBool false -> (VUnit, env)
        | v -> raise (RuntimeError ("类型错误: while 的条件是 " ^ type_of_value v ^ "，需要布尔值", None))
      in
      loop env

  | EIndex (e1, e2) ->
      let v1, _ = eval env e1 in
      let v2, _ = eval env e2 in
      (match v1, v2 with
       | VList vs, VInt idx ->
           (match list_nth_safe vs idx with
            | Some v -> (v, env)
            | None -> raise (RuntimeError ("索引越界: " ^ string_of_int idx, None)))
        | VString s, VInt idx when idx >= 0 && idx < String.length s ->
            (VString (String.make 1 s.[idx]), env)
        | VString _, VInt idx ->
            raise (RuntimeError ("字符串索引越界: " ^ string_of_int idx, None))
        | v1, v2 -> raise (RuntimeError ("类型错误: 索引的对象是 " ^ type_of_value v1 ^ "，索引值是 " ^ type_of_value v2 ^ "，需要列表/字符串和整数", None)))

  | ESlice (e, start, end_) ->
      let v, _ = eval env e in
      let start_idx =
        match start with
        | Some s ->
            let sv, _ = eval env s in
            (match sv with
             | VInt n when n >= 0 -> n
             | VInt n -> raise (RuntimeError ("切片起始索引不能为负数: " ^ string_of_int n, None))
             | sv -> raise (RuntimeError ("类型错误: 切片起始索引是 " ^ type_of_value sv ^ "，需要整数", None)))
        | None -> 0
      in
      let end_idx =
        match end_ with
        | Some e ->
            let ev, _ = eval env e in
            (match ev with
             | VInt n when n >= 0 -> n
             | VInt n -> raise (RuntimeError ("切片结束索引不能为负数: " ^ string_of_int n, None))
             | ev -> raise (RuntimeError ("类型错误: 切片结束索引是 " ^ type_of_value ev ^ "，需要整数", None)))
        | None -> -1
      in
      (match v with
       | VList vs ->
           let len = List.length vs in
           let real_start = min start_idx len in
           let real_end = if end_idx = -1 then len else min end_idx len in
           if real_start > real_end then (VList [], env)
           else
             let rec take n = function
               | [] -> []
               | h :: t -> if n = 0 then [] else h :: take (n - 1) t
             in
             let rec drop n = function
               | [] -> []
               | h :: t -> if n = 0 then h :: t else drop (n - 1) t
             in
             (VList (take (real_end - real_start) (drop real_start vs)), env)
       | VString s ->
           let len = String.length s in
           let real_start = min start_idx len in
           let real_end = if end_idx = -1 then len else min end_idx len in
           if real_start > real_end then (VString "", env)
           else (VString (String.sub s real_start (real_end - real_start)), env)
        | v -> raise (RuntimeError ("类型错误: 切片的对象是 " ^ type_of_value v ^ "，需要列表或字符串", None)))

  | ECtor (c, None) -> (VCtor (c, None), env)
  | ECtor (c, Some e) ->
      let v, _ = eval env e in
      (VCtor (c, Some v), env)
  | ETypeDef _ -> (VUnit, env)

  | ERef e ->
      let v, _ = eval env e in
      (VRef (ref v), env)

  | EDeref e ->
      let v, _ = eval env e in
      (match v with
       | VRef r -> (!r, env)
       | v -> raise (RuntimeError ("类型错误: 解引用需要 ref，但得到 " ^ type_of_value v, None)))

  | EAssign (e1, e2) ->
      (match e1 with
       | EArrayGet (arr, idx) ->
           let v1, _ = eval env arr in
           let v2, _ = eval env idx in
           let v3, _ = eval env e2 in
           (match v1, v2 with
            | VArray a, VInt i when i >= 0 && i < Array.length a ->
                Array.set a i v3; (VUnit, env)
            | VArray _, VInt i ->
                raise (RuntimeError ("数组索引越界: " ^ string_of_int i, None))
            | v1, v2 ->
                raise (RuntimeError ("类型错误: 数组赋值需要 array 和 int，但得到 " ^ type_of_value v1 ^ " 和 " ^ type_of_value v2, None)))
        | ERecordGet (e, field) | EDot (e, field) ->
            let v1, _ = eval env e in
            let v2, _ = eval env e2 in
            (match v1 with
             | VRecord fields ->
                 (match List.assoc_opt field fields with
                  | Some r ->
                      r := v2;
                      (VUnit, env)
                  | None -> raise (RuntimeError ("记录没有字段: " ^ field, None)))
             | v -> raise (RuntimeError ("类型错误: 字段赋值需要 record，但得到 " ^ type_of_value v, None)))
        | _ ->
            let v1, _ = eval env e1 in
            let v2, _ = eval env e2 in
            (match v1 with
             | VRef r -> r := v2; (VUnit, env)
             | v -> raise (RuntimeError ("类型错误: 赋值需要 ref，但得到 " ^ type_of_value v, None))))

  | ERaise e ->
      let v, _ = eval env e in
      raise (Exception_value v)

  | ETry (e, cases) ->
      (try
         eval env e
       with
       | Exception_value v -> eval_match env v cases)

  | EAnnot (e, _) ->
      eval env e

  | ERange (start, end_) ->
      let v1, _ = eval env start in
      let v2, _ = eval env end_ in
      (match v1, v2 with
       | VInt s, VInt e when s <= e ->
           (VList (List.init (e - s + 1) (fun i -> VInt (s + i))), env)
       | VInt s, VInt e when s > e ->
           (VList [], env)
       | v1, v2 ->
           raise (RuntimeError ("类型错误: 范围表达式需要整数，但得到 " ^ type_of_value v1 ^ " 和 " ^ type_of_value v2, None)))

  | EArray es ->
      let vs, env' = eval_list env es in
      (VArray (Array.of_list vs), env')

  | EArrayGet (arr, idx) ->
      let v1, _ = eval env arr in
      let v2, _ = eval env idx in
      (match v1, v2 with
       | VArray a, VInt i when i >= 0 && i < Array.length a ->
           (Array.get a i, env)
       | VArray _, VInt i ->
           raise (RuntimeError ("数组索引越界: " ^ string_of_int i, None))
        | v1, v2 ->
            raise (RuntimeError ("类型错误: 数组索引需要 array 和 int，但得到 " ^ type_of_value v1 ^ " 和 " ^ type_of_value v2, None)))

  | ERecord fields ->
      let vs, env' = eval_record_fields env fields in
      (VRecord (List.map (fun (k, v) -> (k, ref v)) vs), env')

  | ERecordGet (e, field) ->
      let v, _ = eval env e in
      (match v with
       | VRecord fields ->
           (match List.assoc_opt field fields with
            | Some r -> (!r, env)
            | None -> raise (RuntimeError ("记录没有字段: " ^ field, None)))
        | v -> raise (RuntimeError ("类型错误: 字段访问需要 record，但得到 " ^ type_of_value v, None)))

  | ERecordUpdate (e, fields) ->
      let v, _ = eval env e in
      (match v with
       | VRecord old_fields ->
           let new_vs, _ = eval_record_fields env fields in
           let new_fields = List.map (fun (k, v) -> (k, ref v)) new_vs in
           let merged =
             List.map (fun (k, r) ->
               match List.assoc_opt k new_fields with
               | Some new_r -> (k, new_r)
               | None -> (k, r)
             ) old_fields
           in
           let added =
             List.filter (fun (k, _) -> not (List.mem_assoc k old_fields)) new_fields
           in
           (VRecord (merged @ added), env)
        | v -> raise (RuntimeError ("类型错误: 记录更新需要 record，但得到 " ^ type_of_value v, None)))

  | EModule (name, body) ->
      (* 求值模块体，收集导出的绑定 *)
      let module_env = ref [] in
      let rec extract_bindings env expr =
        match expr with
        | ELet (x, v, rest) ->
            let val_v, env' = eval env v in
            module_env := (x, val_v) :: !module_env;
            extract_bindings env' rest
        | ELetRec (x, v, rest) ->
            let val_v, env' = eval env v in
            module_env := (x, val_v) :: !module_env;
            extract_bindings env' rest
        | ETypeDef _ -> extract_bindings env body
        | ESeq (e1, e2) ->
            let _, env' = eval env e1 in
            extract_bindings env' e2
        | _ ->
            let v, _ = eval env expr in
            module_env := ("__value", v) :: !module_env
      in
      extract_bindings env body;
      let module_value = VModule (name, !module_env) in
      (module_value, (name, module_value) :: env)

  | EModuleType (name, sig_expr) ->
      (* 模块类型签名：暂不实现完整签名检查 *)
      (VUnit, env)

  | EOpen name ->
      (match List.assoc_opt name env with
       | Some (VModule (_, module_env)) ->
           (* 将模块的绑定导入到当前环境 *)
           (VUnit, module_env @ env)
       | Some v -> raise (RuntimeError ("open 需要模块，但得到 " ^ type_of_value v, None))
       | None -> raise (RuntimeError ("未定义的模块: " ^ name, None)))

  | EDot (e, field) ->
      let v, _ = eval env e in
      (match v with
       | VModule (_, module_env) ->
           (match List.assoc_opt field module_env with
            | Some fv -> (fv, env)
            | None -> raise (RuntimeError ("模块中未找到字段: " ^ field, None)))
       | VRecord fields ->
           (match List.assoc_opt field fields with
            | Some r -> (!r, env)
            | None -> raise (RuntimeError ("记录没有字段: " ^ field, None)))
       | VCtor (name, None) ->
           (* 构造函数可能被用作模块名，查找环境中的模块 *)
           (match List.assoc_opt name env with
            | Some (VModule (_, module_env)) ->
                (match List.assoc_opt field module_env with
                 | Some fv -> (fv, env)
                 | None -> raise (RuntimeError ("模块中未找到字段: " ^ field, None)))
            | Some v -> raise (RuntimeError ("点号访问需要模块或记录，但得到 " ^ type_of_value v, None))
            | None -> raise (RuntimeError ("未定义的模块: " ^ name, None)))
        | v -> raise (RuntimeError ("点号访问需要模块或记录，但得到 " ^ type_of_value v, None)))

  | ETraitDef (name, params, methods) ->
      let trait_def = {
        Traits.trait_name = name;
        type_params = params;
        methods = List.map (fun (mname, _) ->
          (mname, Types.TArrow (Types.TVar 0, Types.TVar 0))) methods;
      } in
      Traits.define_trait !trait_env trait_def;
      (VUnit, env)

  | ETraitImpl (trait_name, type_name, methods) ->
      (* 1. 求值所有方法并存储到 trait_method_table *)
      let _ = List.iter (fun (mname, mexpr) ->
        let mval, _ = eval env mexpr in
        let key = make_trait_key trait_name mname type_name in
        Hashtbl.replace trait_method_table key mval
      ) methods in
      (* 2. 为每个方法添加分发器到环境 *)
      let dispatch_env = List.fold_left (fun env_acc (mname, _) ->
        if List.mem_assoc mname env_acc then env_acc
        else
          let dispatch = VBuiltin (mname, fun env arg ->
            let arg_type = match arg with
              | VInt _ -> "int"
              | VBool _ -> "bool"
              | VString _ -> "string"
              | VList _ -> "list"
              | VTuple _ -> "tuple"
              | _ -> "unknown"
            in
            let key = make_trait_key trait_name mname arg_type in
            match Hashtbl.find_opt trait_method_table key with
            | Some mval -> apply_value env mval arg
            | None -> raise (RuntimeError ("未找到实现: " ^ trait_name ^ "." ^ mname ^ " for " ^ arg_type, None))
          ) in
          (mname, dispatch) :: env_acc
      ) env methods in
      (VUnit, dispatch_env)

  | ESpawn e ->
      let v, _ = eval env e in
      (match v with
       | VFun _ | VBuiltin _ ->
           let f () =
             let result, _ = apply_value env v VUnit in
             result
           in
           (Actor.spawn_actor f, env)
       | _ -> raise (RuntimeError ("spawn 需要函数", None)))

  | ESend (pid_e, msg_e) ->
      let pid_v, _ = eval env pid_e in
      let msg_v, _ = eval env msg_e in
      (match pid_v with
       | VInt pid ->
           Actor.send_message pid msg_v;
           (VUnit, env)
       | _ -> raise (RuntimeError ("send 需要整数 pid", None)))

  | EReceive ->
      let msg = Actor.receive_message () in
      (msg, env)

  | EEffectDef (name, ops) ->
      (* 效果定义注册到环境，用于后续 perform 查找 *)
      let effect_env = List.fold_left (fun env_acc op ->
        (op, VBuiltin (op, fun env arg -> raise (RuntimeError ("效果 " ^ op ^ " 未在 handle 中处理", None)))) :: env_acc
      ) env ops in
      (VUnit, effect_env)

  | EPerform (op, arg) ->
      let v, _ = eval env arg in
      (match List.assoc_opt op env with
       | Some handler ->
           let resume_fn = VBuiltin ("resume", fun env arg -> (arg, env)) in
           let partial1, _ = apply_value env handler v in
           let result, _ = apply_value env partial1 resume_fn in
           (result, env)
       | None -> raise (RuntimeError ("未处理的效果: " ^ op, None)))

  | EHandle (e, handlers) ->
      (* 将 handler 转换为 curried 函数并添加到环境 *)
      let handler_env = List.fold_left (fun env_acc (op, arg_name, k_name, body) ->
        let handler_fn = VFun (None, arg_name, EFun (k_name, body), env_acc) in
        (op, handler_fn) :: env_acc
      ) env handlers in
      eval handler_env e

  and eval_list env es =
  match es with
  | [] -> ([], env)
  | e :: rest ->
      let v, env' = eval env e in
      let vs, env'' = eval_list env' rest in
      (v :: vs, env'')

and eval_record_fields env fields =
  match fields with
  | [] -> ([], env)
  | (name, e) :: rest ->
      let v, env' = eval env e in
      let vs, env'' = eval_record_fields env' rest in
      ((name, v) :: vs, env'')

and eval_match env v cases =
  match cases with
  | [] -> raise (RuntimeError ("匹配失败: 没有匹配的模式", None))
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
  | PRecord fields, VRecord record_fields ->
      let rec match_record = function
        | [] -> Some []
        | (name, pat) :: rest ->
            (match List.assoc_opt name record_fields with
             | Some ref_val ->
                 (match match_pattern pat !ref_val with
                  | Some b1 ->
                      (match match_record rest with
                       | Some b2 -> Some (b1 @ b2)
                       | None -> None)
                  | None -> None)
             | None -> None)
      in
      match_record fields
  | PCons (p1, p2), VList (h :: t) ->
      (match match_pattern p1 h with
       | Some b1 ->
           (match match_pattern p2 (VList t) with
            | Some b2 -> Some (b1 @ b2)
            | None -> None)
       | None -> None)
  | PCtor (c, None), VCtor (d, None) when c = d -> Some []
  | PCtor (c, Some p), VCtor (d, Some v) when c = d -> match_pattern p v
  | PCtor _, _ -> None
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
  ; ( "show",
      Types.Forall
        ( [0],
          Types.TArrow (Types.TVar 0, Types.TString) ) )
  ; ( "string_length",
      Types.Forall
        ( [],
          Types.TArrow (Types.TString, Types.TInt) ) )
  ; ( "string_get",
      Types.Forall
        ( [],
          Types.TArrow (Types.TString, Types.TArrow (Types.TInt, Types.TChar)) ) )
  ; ( "string_sub",
      Types.Forall
        ( [],
          Types.TArrow (Types.TString, Types.TArrow (Types.TInt, Types.TArrow (Types.TInt, Types.TString))) ) )
  ; ( "read_file",
      Types.Forall
        ( [],
          Types.TArrow (Types.TString, Types.TString) ) )
  ; ( "write_file",
      Types.Forall
        ( [],
          Types.TArrow (Types.TString, Types.TArrow (Types.TString, Types.TUnit)) ) )
  ; ( "read_line",
      Types.Forall
        ( [],
          Types.TArrow (Types.TUnit, Types.TString) ) )
  ; ( "print_string",
      Types.Forall
        ( [],
          Types.TArrow (Types.TString, Types.TUnit) ) )
  ; ( "map",
      Types.Forall
        ( [0; 1],
          Types.TArrow
            ( Types.TArrow (Types.TVar 0, Types.TVar 1),
              Types.TArrow (Types.TList (Types.TVar 0), Types.TList (Types.TVar 1)) ) ) )
  ; ( "filter",
      Types.Forall
        ( [0],
          Types.TArrow
            ( Types.TArrow (Types.TVar 0, Types.TBool),
              Types.TArrow (Types.TList (Types.TVar 0), Types.TList (Types.TVar 0)) ) ) )
  ; ( "fold",
      Types.Forall
        ( [0; 1],
          Types.TArrow
            ( Types.TArrow (Types.TVar 1, Types.TArrow (Types.TVar 0, Types.TVar 1)),
              Types.TArrow (Types.TVar 1, Types.TArrow (Types.TList (Types.TVar 0), Types.TVar 1)) ) ) )
  ; ( "range",
      Types.Forall
        ( [],
          Types.TArrow
            ( Types.TInt,
              Types.TArrow (Types.TInt, Types.TList Types.TInt) ) ) )
  ; ( "sum",
      Types.Forall
        ( [],
          Types.TArrow (Types.TList Types.TInt, Types.TInt) ) )
  ; ( "reverse",
      Types.Forall
        ( [0],
          Types.TArrow (Types.TList (Types.TVar 0), Types.TList (Types.TVar 0)) ) )
  ; ( "append",
      Types.Forall
        ( [0],
          Types.TArrow
            ( Types.TList (Types.TVar 0),
              Types.TArrow (Types.TList (Types.TVar 0), Types.TList (Types.TVar 0)) ) ) )
  ; ( "timeit",
      Types.Forall
        ( [0],
          Types.TArrow
            ( Types.TArrow (Types.TUnit, Types.TVar 0),
              Types.TVar 0 ) ) )
  ; ( "string_trim",
      Types.Forall ([], Types.TArrow (Types.TString, Types.TString)) )
  ; ( "string_uppercase",
      Types.Forall ([], Types.TArrow (Types.TString, Types.TString)) )
  ; ( "string_lowercase",
      Types.Forall ([], Types.TArrow (Types.TString, Types.TString)) )
  ; ( "string_concat",
      Types.Forall ([], Types.TArrow (Types.TTuple [Types.TString; Types.TList Types.TString], Types.TString)) )
  ; ( "string_split",
      Types.Forall ([], Types.TArrow (Types.TTuple [Types.TString; Types.TString], Types.TList Types.TString)) )
  ; ( "string_contains",
      Types.Forall ([], Types.TArrow (Types.TTuple [Types.TString; Types.TString], Types.TBool)) )
  ; ( "string_replace",
      Types.Forall ([], Types.TArrow (Types.TTuple [Types.TString; Types.TString; Types.TString], Types.TString)) )
  ; ( "take",
      Types.Forall ([0], Types.TArrow (Types.TTuple [Types.TInt; Types.TList (Types.TVar 0)], Types.TList (Types.TVar 0))) )
  ; ( "drop",
      Types.Forall ([0], Types.TArrow (Types.TTuple [Types.TInt; Types.TList (Types.TVar 0)], Types.TList (Types.TVar 0))) )
  ; ( "find",
      Types.Forall ([0], Types.TArrow (Types.TTuple [Types.TArrow (Types.TVar 0, Types.TBool); Types.TList (Types.TVar 0)], Types.TADT ("option", [Types.TVar 0]))) )
  ; ( "exists",
      Types.Forall ([0], Types.TArrow (Types.TTuple [Types.TArrow (Types.TVar 0, Types.TBool); Types.TList (Types.TVar 0)], Types.TBool)) )
  ; ( "forall",
      Types.Forall ([0], Types.TArrow (Types.TTuple [Types.TArrow (Types.TVar 0, Types.TBool); Types.TList (Types.TVar 0)], Types.TBool)) )
  ; ( "sort",
      Types.Forall ([0], Types.TArrow (Types.TList (Types.TVar 0), Types.TList (Types.TVar 0))) )
  ; ( "zip",
      Types.Forall ([0; 1], Types.TArrow (Types.TTuple [Types.TList (Types.TVar 0); Types.TList (Types.TVar 1)], Types.TList (Types.TTuple [Types.TVar 0; Types.TVar 1]))) )
  ; ( "abs",
      Types.Forall ([], Types.TArrow (Types.TInt, Types.TInt)) )
  ; ( "min",
      Types.Forall ([0], Types.TArrow (Types.TTuple [Types.TVar 0; Types.TVar 0], Types.TVar 0)) )
  ; ( "max",
      Types.Forall ([0], Types.TArrow (Types.TTuple [Types.TVar 0; Types.TVar 0], Types.TVar 0)) )
  ; ( "int_of_string",
      Types.Forall ([], Types.TArrow (Types.TString, Types.TInt)) )
  ; ( "string_of_int",
      Types.Forall ([], Types.TArrow (Types.TInt, Types.TString)) )
  ; ( "int_of_char",
      Types.Forall ([], Types.TArrow (Types.TChar, Types.TInt)) )
   ; ( "char_of_int",
      Types.Forall ([], Types.TArrow (Types.TInt, Types.TChar)) )
   ; ( "sqrt",
      Types.Forall ([], Types.TArrow (Types.TInt, Types.TInt)) )
   ; ( "pow",
      Types.Forall ([], Types.TArrow (Types.TTuple [Types.TInt; Types.TInt], Types.TInt)) )
   ; ( "random_int",
      Types.Forall ([], Types.TArrow (Types.TTuple [Types.TInt; Types.TInt], Types.TInt)) )
   ; ( "current_time",
      Types.Forall ([], Types.TArrow (Types.TUnit, Types.TInt)) )
   ; ( "sleep",
      Types.Forall ([], Types.TArrow (Types.TInt, Types.TUnit)) )
   ; ( "file_exists",
      Types.Forall ([], Types.TArrow (Types.TString, Types.TBool)) )
   ; ( "file_size",
      Types.Forall ([], Types.TArrow (Types.TString, Types.TInt)) )
   ; ( "delete_file",
      Types.Forall ([], Types.TArrow (Types.TString, Types.TUnit)) )
   ; ( "list_directory",
      Types.Forall ([], Types.TArrow (Types.TString, Types.TList Types.TString)) )
   ; ( "get_env",
      Types.Forall ([], Types.TArrow (Types.TString, Types.TADT ("option", [Types.TString]))) )
     ; ( "system_command",
        Types.Forall ([], Types.TArrow (Types.TString, Types.TInt)) )
     ; ( "regex_match",
        Types.Forall ([], Types.TArrow (Types.TTuple [Types.TString; Types.TString], Types.TBool)) )
     ; ( "regex_replace",
        Types.Forall ([], Types.TArrow (Types.TTuple [Types.TString; Types.TString; Types.TString], Types.TString)) )
     ; ( "regex_split",
        Types.Forall ([], Types.TArrow (Types.TTuple [Types.TString; Types.TString], Types.TList Types.TString)) )
     ]

let builtin_env =
  let import_func env v =
    match v with
    | VString filename ->
        let content =
          try Core.In_channel.read_all filename
          with Sys_error msg -> raise (RuntimeError ("无法导入文件: " ^ msg, None))
        in
        let lexbuf = Lexing.from_string content in
        let expr = Parser.prog Lexer.read lexbuf in
        let _, env' = eval env expr in
        (VUnit, env')
    | _ -> raise (RuntimeError ("import: 需要字符串文件名", None))
  in
  [ ( "head",
      VBuiltin
        ( "head",
          fun env -> function
          | VList (h :: _) -> (h, env)
          | VList [] -> raise (RuntimeError ("head: 空列表", None))
          | _ -> raise (RuntimeError ("head: 需要列表", None)) ) )
  ; ( "tail",
      VBuiltin
        ( "tail",
          fun env -> function
          | VList (_ :: t) -> (VList t, env)
          | VList [] -> raise (RuntimeError ("tail: 空列表", None))
          | _ -> raise (RuntimeError ("tail: 需要列表", None)) ) )
  ; ( "length",
      VBuiltin
        ( "length",
          fun env -> function
          | VList l -> (VInt (List.length l), env)
          | VString s -> (VInt (String.length s), env)
          | _ -> raise (RuntimeError ("length: 需要列表或字符串", None)) ) )
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
  ; ( "show",
      VBuiltin
        ( "show",
          fun env v ->
            (VString (string_of_value v), env) ) )
  ; ( "string_length",
      VBuiltin
        ( "string_length",
          fun env -> function
          | VString s -> (VInt (String.length s), env)
          | v -> raise (RuntimeError ("string_length: 需要字符串，但得到 " ^ type_of_value v, None)) ) )
  ; ( "string_get",
      VBuiltin
        ( "string_get",
          fun env s ->
            (VBuiltin
               ( "string_get'",
                 fun env idx ->
                   match s, idx with
                   | VString s, VInt i when i >= 0 && i < String.length s ->
                       (VChar s.[i], env)
                   | VString _, VInt i ->
                       raise (RuntimeError ("string_get: 索引越界: " ^ string_of_int i, None))
                   | VString _, v ->
                       raise (RuntimeError ("string_get: 索引需要整数，但得到 " ^ type_of_value v, None))
                   | v, _ ->
                       raise (RuntimeError ("string_get: 需要字符串，但得到 " ^ type_of_value v, None)) ),
             env) ) )
  ; ( "string_sub",
      VBuiltin
        ( "string_sub",
          fun env s ->
            (VBuiltin
               ( "string_sub'",
                 fun env start ->
                   (VBuiltin
                      ( "string_sub''",
                        fun env len ->
                          match s, start, len with
                          | VString s, VInt start, VInt len when start >= 0 && len >= 0 && start + len <= String.length s ->
                              (VString (String.sub s start len), env)
                          | VString _, VInt _, VInt _ ->
                              raise (RuntimeError ("string_sub: 索引越界", None))
                          | VString _, VInt _, v ->
                              raise (RuntimeError ("string_sub: 长度需要整数，但得到 " ^ type_of_value v, None))
                          | VString _, v, _ ->
                              raise (RuntimeError ("string_sub: 起始需要整数，但得到 " ^ type_of_value v, None))
                          | v, _, _ ->
                              raise (RuntimeError ("string_sub: 需要字符串，但得到 " ^ type_of_value v, None)) ),
                    env) ),
             env) ) )
  ; ( "read_file",
      VBuiltin
        ( "read_file",
          fun env -> function
          | VString filename ->
              let content =
                try Core.In_channel.read_all filename
                with Sys_error msg -> raise (RuntimeError ("无法读取文件: " ^ msg, None))
              in
              (VString content, env)
          | v -> raise (RuntimeError ("read_file: 需要字符串文件名，但得到 " ^ type_of_value v, None)) ) )
  ; ( "write_file",
      VBuiltin
        ( "write_file",
          fun env filename ->
            (VBuiltin
               ( "write_file'",
                 fun env content ->
                   match filename, content with
                   | VString filename, VString content ->
                       (try
                          Core.Out_channel.write_all filename ~data:content;
                          (VUnit, env)
                        with Sys_error msg -> raise (RuntimeError ("无法写入文件: " ^ msg, None)))
                   | VString _, v ->
                       raise (RuntimeError ("write_file: 内容需要字符串，但得到 " ^ type_of_value v, None))
                   | v, _ ->
                       raise (RuntimeError ("write_file: 文件名需要字符串，但得到 " ^ type_of_value v, None)) ),
             env) ) )
  ; ( "read_line",
      VBuiltin
        ( "read_line",
          fun env -> function
          | VUnit ->
              let line =
                try input_line stdin
                with End_of_file -> ""
              in
              (VString line, env)
          | v -> raise (RuntimeError ("read_line: 需要 unit，但得到 " ^ type_of_value v, None)) ) )
  ; ( "print_string",
      VBuiltin
        ( "print_string",
          fun env -> function
          | VString s ->
              print_string s;
              (VUnit, env)
          | v -> raise (RuntimeError ("print_string: 需要字符串，但得到 " ^ type_of_value v, None)) ) )
  ; ( "map",
      VBuiltin
        ( "map",
          fun env f ->
            (VBuiltin
               ( "map'",
                 fun env xs ->
                   match xs with
                   | VList items ->
                       let results =
                         List.map (fun item -> let v, _ = apply_value env f item in v) items
                       in
                       (VList results, env)
                   | v -> raise (RuntimeError ("map: 第二个参数必须是列表，但得到 " ^ type_of_value v, None)) ),
             env)
        ) )
  ; ( "filter",
      VBuiltin
        ( "filter",
          fun env f ->
            (VBuiltin
               ( "filter'",
                 fun env xs ->
                   match xs with
                   | VList items ->
                       let results =
                         List.filter
                           (fun item ->
                             let v, _ = apply_value env f item in
                             match v with
                             | VBool b -> b
                              | v ->
                                  raise
                                    (RuntimeError
                                       ( "filter: 谓词函数必须返回布尔值，但得到 "
                                       ^ type_of_value v, None)))
                            items
                       in
                       (VList results, env)
                   | v ->
                       raise (RuntimeError ("filter: 第二个参数必须是列表，但得到 " ^ type_of_value v, None)) ),
             env)
        ) )
  ; ( "fold",
      VBuiltin
        ( "fold",
          fun env f ->
            (VBuiltin
               ( "fold'",
                 fun env acc ->
                   (VBuiltin
                      ( "fold''",
                        fun env xs ->
                          match xs with
                          | VList items ->
                              let result =
                                List.fold_left
                                  (fun acc item ->
                                    let f_acc, _ = apply_value env f acc in
                                    match f_acc with
                                    | VFun _ | VBuiltin _ ->
                                        let v, _ = apply_value env f_acc item in
                                    v
                                    | v ->
                                         raise
                                           (RuntimeError
                                              ( "fold:  folding 函数必须接受两个参数，但得到 "
                                              ^ type_of_value v, None)))
                                  acc items
                              in
                              (result, env)
                           | v ->
                               raise
                                 (RuntimeError
                                    ("fold: 第三个参数必须是列表，但得到 " ^ type_of_value v, None)) ),
                      env)
                    ),
              env)
         ) )
  ; ( "range",
      VBuiltin
        ( "range",
          fun env start ->
            (VBuiltin
               ( "range'",
                 fun env end_val ->
                   match start, end_val with
                   | VInt s, VInt e ->
                       let rec build_range i acc =
                         if i > e then List.rev acc
                         else build_range (i + 1) (VInt i :: acc)
                       in
                       let nums = build_range s [] in
                       (VList nums, env)
                   | _ ->
                       raise (RuntimeError ("range: 需要整数参数", None)) ),
             env) ) )
  ; ( "sum",
      VBuiltin
        ( "sum",
          fun env xs ->
            match xs with
            | VList items ->
                let total =
                  List.fold_left
                    (fun acc item ->
                      match item with
                      | VInt n -> acc + n
                      | _ -> raise (RuntimeError ("sum: 列表元素必须是整数", None)))
                    0 items
                in
                (VInt total, env)
            | _ ->
                raise (RuntimeError ("sum: 需要列表", None)) ) )
  ; ( "reverse",
      VBuiltin
        ( "reverse",
          fun env xs ->
            match xs with
            | VList items -> (VList (List.rev items), env)
            | _ -> raise (RuntimeError ("reverse: 需要列表", None)) ) )
  ; ( "append",
      VBuiltin
        ( "append",
          fun env xs ->
            (VBuiltin
               ( "append'",
                 fun env ys ->
                   match xs, ys with
                   | VList a, VList b -> (VList (a @ b), env)
                   | _ -> raise (RuntimeError ("append: 需要两个列表", None)) ),
             env) ) )
  ; ( "timeit",
      VBuiltin
        ( "timeit",
          fun env f ->
            match f with
            | VFun _ | VBuiltin _ ->
                        let start = Core.Time_float.now () in
                        let result, _ = apply_value env f (VTuple []) in
                        let elapsed = Core.Time_float.diff (Core.Time_float.now ()) start in
                        let ms = Core.Time_float.Span.to_ms elapsed in
                        Printf.printf "[timeit] %.4f ms\n%!" ms;
                        (result, env)
            | _ ->
                raise (RuntimeError ("timeit: 需要函数", None)) ) )
  ; ( "string_trim",
      VBuiltin
        ( "string_trim",
          fun env -> function
          | VString s -> (VString (String.trim s), env)
          | v -> raise (RuntimeError ("string_trim: 需要字符串，但得到 " ^ type_of_value v, None)) ) )
  ; ( "string_uppercase",
      VBuiltin
        ( "string_uppercase",
          fun env -> function
          | VString s -> (VString (String.uppercase_ascii s), env)
          | v -> raise (RuntimeError ("string_uppercase: 需要字符串，但得到 " ^ type_of_value v, None)) ) )
  ; ( "string_lowercase",
      VBuiltin
        ( "string_lowercase",
          fun env -> function
          | VString s -> (VString (String.lowercase_ascii s), env)
          | v -> raise (RuntimeError ("string_lowercase: 需要字符串，但得到 " ^ type_of_value v, None)) ) )
  ; ( "string_concat",
      VBuiltin
        ( "string_concat",
          fun env -> function
          | VTuple [VString sep; VList items] ->
              let strs = List.map (function VString s -> s | _ -> raise (RuntimeError ("string_concat: 列表元素必须是字符串", None))) items in
              (VString (String.concat sep strs), env)
          | v -> raise (RuntimeError ("string_concat: 需要 (分隔符, 字符串列表) 元组，但得到 " ^ type_of_value v, None)) ) )
  ; ( "string_split",
      VBuiltin
        ( "string_split",
          fun env -> function
          | VTuple [VString sep; VString s] ->
              let parts = Core.String.split s ~on:(sep.[0]) in
              (VList (List.map (fun p -> VString p) parts), env)
          | v -> raise (RuntimeError ("string_split: 需要 (分隔符, 字符串) 元组，但得到 " ^ type_of_value v, None)) ) )
  ; ( "string_contains",
      VBuiltin
        ( "string_contains",
          fun env -> function
          | VTuple [VString substr; VString s] -> (VBool (Core.String.is_substring s ~substring:substr), env)
          | v -> raise (RuntimeError ("string_contains: 需要 (子串, 字符串) 元组，但得到 " ^ type_of_value v, None)) ) )
  ; ( "string_replace",
      VBuiltin
        ( "string_replace",
          fun env -> function
          | VTuple [VString old_s; VString new_s; VString s] -> (VString (Core.String.substr_replace_all s ~pattern:old_s ~with_:new_s), env)
          | v -> raise (RuntimeError ("string_replace: 需要 (旧字符串, 新字符串, 字符串) 元组，但得到 " ^ type_of_value v, None)) ) )
  ; ( "take",
      VBuiltin
        ( "take",
          fun env -> function
          | VTuple [VInt n; VList items] when n >= 0 -> (VList (Core.List.take items n), env)
          | VTuple [VInt n; VList _] when n < 0 -> raise (RuntimeError ("take: 参数不能为负数", None))
          | v -> raise (RuntimeError ("take: 需要 (整数, 列表) 元组，但得到 " ^ type_of_value v, None)) ) )
  ; ( "drop",
      VBuiltin
        ( "drop",
          fun env -> function
          | VTuple [VInt n; VList items] when n >= 0 -> (VList (Core.List.drop items n), env)
          | VTuple [VInt n; VList _] when n < 0 -> raise (RuntimeError ("drop: 参数不能为负数", None))
          | v -> raise (RuntimeError ("drop: 需要 (整数, 列表) 元组，但得到 " ^ type_of_value v, None)) ) )
  ; ( "find",
      VBuiltin
        ( "find",
          fun env -> function
          | VTuple [f; VList items] ->
              let rec find_loop = function
                | [] -> (VCtor ("None", None), env)
                | h :: t ->
                    let v, _ = apply_value env f h in
                    (match v with
                     | VBool true -> (VCtor ("Some", Some h), env)
                     | VBool false -> find_loop t
                     | _ -> raise (RuntimeError ("find: 谓词函数必须返回布尔值", None)))
              in
              find_loop items
          | v -> raise (RuntimeError ("find: 需要 (函数, 列表) 元组，但得到 " ^ type_of_value v, None)) ) )
  ; ( "exists",
      VBuiltin
        ( "exists",
          fun env -> function
          | VTuple [f; VList items] ->
              let rec exists_loop = function
                | [] -> (VBool false, env)
                | h :: t ->
                    let v, _ = apply_value env f h in
                    (match v with
                     | VBool true -> (VBool true, env)
                     | VBool false -> exists_loop t
                     | _ -> raise (RuntimeError ("exists: 谓词函数必须返回布尔值", None)))
              in
              exists_loop items
          | v -> raise (RuntimeError ("exists: 需要 (函数, 列表) 元组，但得到 " ^ type_of_value v, None)) ) )
  ; ( "forall",
      VBuiltin
        ( "forall",
          fun env -> function
          | VTuple [f; VList items] ->
              let rec forall_loop = function
                | [] -> (VBool true, env)
                | h :: t ->
                    let v, _ = apply_value env f h in
                    (match v with
                     | VBool true -> forall_loop t
                     | VBool false -> (VBool false, env)
                     | _ -> raise (RuntimeError ("forall: 谓词函数必须返回布尔值", None)))
              in
              forall_loop items
          | v -> raise (RuntimeError ("forall: 需要 (函数, 列表) 元组，但得到 " ^ type_of_value v, None)) ) )
  ; ( "sort",
      VBuiltin
        ( "sort",
          fun env -> function
          | VList items ->
              let sorted = Core.List.sort ~compare:(fun a b -> match a, b with VInt x, VInt y -> Int.compare x y | VString x, VString y -> String.compare x y | _ -> raise (RuntimeError ("sort: 列表元素必须是可比较的整数或字符串", None))) items in
              (VList sorted, env)
          | v -> raise (RuntimeError ("sort: 需要列表，但得到 " ^ type_of_value v, None)) ) )
  ; ( "zip",
      VBuiltin
        ( "zip",
          fun env -> function
          | VTuple [VList a; VList b] ->
              let zipped = Core.List.map2_exn ~f:(fun x y -> VTuple [x; y]) a b in
              (VList zipped, env)
          | v -> raise (RuntimeError ("zip: 需要 (列表, 列表) 元组，但得到 " ^ type_of_value v, None)) ) )
  ; ( "abs",
      VBuiltin
        ( "abs",
          fun env -> function
          | VInt n -> (VInt (Int.abs n), env)
          | v -> raise (RuntimeError ("abs: 需要整数，但得到 " ^ type_of_value v, None)) ) )
  ; ( "min",
      VBuiltin
        ( "min",
          fun env -> function
          | VTuple [VInt x; VInt y] -> (VInt (Int.min x y), env)
          | VTuple [VString x; VString y] -> (VString (if String.compare x y <= 0 then x else y), env)
          | v -> raise (RuntimeError ("min: 需要两个整数或两个字符串的元组，但得到 " ^ type_of_value v, None)) ) )
  ; ( "max",
      VBuiltin
        ( "max",
          fun env -> function
          | VTuple [VInt x; VInt y] -> (VInt (Int.max x y), env)
          | VTuple [VString x; VString y] -> (VString (if String.compare x y >= 0 then x else y), env)
          | v -> raise (RuntimeError ("max: 需要两个整数或两个字符串的元组，但得到 " ^ type_of_value v, None)) ) )
  ; ( "int_of_string",
      VBuiltin
        ( "int_of_string",
          fun env -> function
          | VString s -> (try (VInt (int_of_string s), env) with Failure _ -> raise (RuntimeError ("int_of_string: 无效的整数字符串: " ^ s, None)))
          | v -> raise (RuntimeError ("int_of_string: 需要字符串，但得到 " ^ type_of_value v, None)) ) )
  ; ( "string_of_int",
      VBuiltin
        ( "string_of_int",
          fun env -> function
          | VInt n -> (VString (Int.to_string n), env)
          | v -> raise (RuntimeError ("string_of_int: 需要整数，但得到 " ^ type_of_value v, None)) ) )
  ; ( "int_of_char",
      VBuiltin
        ( "int_of_char",
          fun env -> function
          | VChar c -> (VInt (Char.code c), env)
          | v -> raise (RuntimeError ("int_of_char: 需要字符，但得到 " ^ type_of_value v, None)) ) )
   ; ( "char_of_int",
      VBuiltin
        ( "char_of_int",
          fun env -> function
          | VInt n -> if n >= 0 && n <= 255 then (VChar (Char.chr n), env) else raise (RuntimeError ("char_of_int: 超出字符范围 (0-255)", None))
          | v -> raise (RuntimeError ("char_of_int: 需要整数，但得到 " ^ type_of_value v, None)) ) )
   ; ( "sqrt",
      VBuiltin
        ( "sqrt",
          fun env -> function
          | VInt n -> if n >= 0 then (VInt (int_of_float (sqrt (float_of_int n))), env) else raise (RuntimeError ("sqrt: 不能对负数开方", None))
          | v -> raise (RuntimeError ("sqrt: 需要整数，但得到 " ^ type_of_value v, None)) ) )
   ; ( "pow",
      VBuiltin
        ( "pow",
          fun env -> function
          | VTuple [VInt base; VInt exp] -> (VInt (int_of_float ((float_of_int base) ** (float_of_int exp))), env)
          | v -> raise (RuntimeError ("pow: 需要两个整数，但得到 " ^ type_of_value v, None)) ) )
   ; ( "random_int",
      VBuiltin
        ( "random_int",
          fun env -> function
          | VTuple [VInt min; VInt max] ->
              if min <= max then (VInt (min + Random.int (max - min + 1)), env)
              else raise (RuntimeError ("random_int: 最小值不能大于最大值", None))
          | v -> raise (RuntimeError ("random_int: 需要两个整数，但得到 " ^ type_of_value v, None)) ) )
   ; ( "current_time",
      VBuiltin
        ( "current_time",
          fun env -> function
          | VTuple [] -> (VInt (int_of_float (Unix.gettimeofday ())), env)
          | VUnit -> (VInt (int_of_float (Unix.gettimeofday ())), env)
          | v -> raise (RuntimeError ("current_time: 需要 unit，但得到 " ^ type_of_value v, None)) ) )
   ; ( "sleep",
      VBuiltin
        ( "sleep",
          fun env -> function
          | VInt ms -> (Unix.sleepf (float_of_int ms /. 1000.0); (VUnit, env))
          | v -> raise (RuntimeError ("sleep: 需要整数（毫秒），但得到 " ^ type_of_value v, None)) ) )
   ; ( "file_exists",
      VBuiltin
        ( "file_exists",
          fun env -> function
          | VString path -> (VBool (Stdlib.Sys.file_exists path), env)
          | v -> raise (RuntimeError ("file_exists: 需要字符串，但得到 " ^ type_of_value v, None)) ) )
   ; ( "file_size",
      VBuiltin
        ( "file_size",
          fun env -> function
          | VString path ->
              (try
                 let ic = Stdlib.open_in path in
                 let size = Stdlib.in_channel_length ic in
                 Stdlib.close_in ic;
                 (VInt size, env)
               with _ -> raise (RuntimeError ("file_size: 无法获取文件大小: " ^ path, None)))
          | v -> raise (RuntimeError ("file_size: 需要字符串，但得到 " ^ type_of_value v, None)) ) )
   ; ( "delete_file",
      VBuiltin
        ( "delete_file",
          fun env -> function
          | VString path -> (Stdlib.Sys.remove path; (VUnit, env))
          | v -> raise (RuntimeError ("delete_file: 需要字符串，但得到 " ^ type_of_value v, None)) ) )
   ; ( "list_directory",
      VBuiltin
        ( "list_directory",
          fun env -> function
          | VString path ->
              (try
                 let files = Stdlib.Sys.readdir path |> Array.to_list in
                 (VList (List.map (fun f -> VString f) files), env)
               with _ -> raise (RuntimeError ("list_directory: 无法读取目录: " ^ path, None)))
          | v -> raise (RuntimeError ("list_directory: 需要字符串，但得到 " ^ type_of_value v, None)) ) )
   ; ( "get_env",
      VBuiltin
        ( "get_env",
          fun env -> function
          | VString var ->
              (try
                 let value = Stdlib.Sys.getenv var in
                 (VCtor ("Some", Some (VString value)), env)
               with Not_found -> (VCtor ("None", None), env))
          | v -> raise (RuntimeError ("get_env: 需要字符串，但得到 " ^ type_of_value v, None)) ) )
   ; ( "system_command",
      VBuiltin
        ( "system_command",
          fun env -> function
          | VString cmd -> 
              let status = Unix.system cmd in
              let code = match status with
                | Unix.WEXITED n -> n
                | Unix.WSIGNALED _ | Unix.WSTOPPED _ -> -1
              in
              (VInt code, env)
          | v -> raise (RuntimeError ("system_command: 需要字符串，但得到 " ^ type_of_value v, None)) ) )
     ; ( "regex_match",
       VBuiltin
         ( "regex_match",
           fun env -> function
           | VTuple [VString pattern; VString text] ->
               (try
                  let re = Str.regexp pattern in
                  (VBool (Str.string_match re text 0), env)
                with _ -> raise (RuntimeError ("regex_match: 无效的正则表达式: " ^ pattern, None)))
           | v -> raise (RuntimeError ("regex_match: 需要(模式, 文本)，但得到 " ^ type_of_value v, None)) ) )
    ; ( "regex_replace",
       VBuiltin
         ( "regex_replace",
           fun env -> function
           | VTuple [VString pattern; VString replacement; VString text] ->
               (try
                  let re = Str.regexp pattern in
                  (VString (Str.global_replace re replacement text), env)
                with _ -> raise (RuntimeError ("regex_replace: 无效的正则表达式: " ^ pattern, None)))
           | v -> raise (RuntimeError ("regex_replace: 需要(模式, 替换, 文本)，但得到 " ^ type_of_value v, None)) ) )
    ; ( "regex_split",
       VBuiltin
         ( "regex_split",
           fun env -> function
           | VTuple [VString pattern; VString text] ->
               (try
                  let re = Str.regexp pattern in
                  let parts = Str.split re text in
                  (VList (List.map (fun s -> VString s) parts), env)
                with _ -> raise (RuntimeError ("regex_split: 无效的正则表达式: " ^ pattern, None)))
           | v -> raise (RuntimeError ("regex_split: 需要(模式, 文本)，但得到 " ^ type_of_value v, None)) ) )
    ]

let run expr =
  let v, _ = eval builtin_env expr in
  v
