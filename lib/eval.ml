(** 求值器核心模块 *)

open Ast
open Eval_helpers

(** 注册内置 trait 实现 *)
let () = init_traits ()

(** 安全的列表索引 *)
let list_nth_safe = Eval_helpers.list_nth_safe

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
      let* (v, _) = eval extended_env body in
      Ok (v, env)
  | VBuiltin (_, f) -> f env arg
  | v -> Error ("应用需要函数，但得到 " ^ type_of_value v)

(** eval 返回 (值, 新环境) *)
and eval env expr =
  match expr with
  | EInt n -> Ok (VInt n, env)
  | EBool b -> Ok (VBool b, env)
  | EChar c -> Ok (VChar c, env)
  | EString s -> Ok (VString s, env)
  | EList es ->
      let* (vs, env') = eval_list env es in
      Ok (VList vs, env')
  | ETuple es ->
      let* (vs, env') = eval_list env es in
      Ok (VTuple vs, env')
  | EVar x ->
      let* v = lookup env x in
      Ok (v, env)
  
  | EAdd (e1, e2) ->
      let* (v1, _) = eval env e1 in
      let* (v2, _) = eval env e2 in
      (match v1, v2 with
       | VInt a, VInt b -> Ok (VInt (a + b), env)
       | VInt _, v2 -> Error ("类型错误: + 的右操作数是 " ^ type_of_value v2 ^ "，需要整数")
       | v1, _ -> Error ("类型错误: + 的左操作数是 " ^ type_of_value v1 ^ "，需要整数"))
  
  | ESub (e1, e2) ->
      let* (v1, _) = eval env e1 in
      let* (v2, _) = eval env e2 in
      (match v1, v2 with
       | VInt a, VInt b -> Ok (VInt (a - b), env)
       | VInt _, v2 -> Error ("类型错误: - 的右操作数是 " ^ type_of_value v2 ^ "，需要整数")
       | v1, _ -> Error ("类型错误: - 的左操作数是 " ^ type_of_value v1 ^ "，需要整数"))
  
  | EMul (e1, e2) ->
      let* (v1, _) = eval env e1 in
      let* (v2, _) = eval env e2 in
      (match v1, v2 with
       | VInt a, VInt b -> Ok (VInt (a * b), env)
       | VInt _, v2 -> Error ("类型错误: * 的右操作数是 " ^ type_of_value v2 ^ "，需要整数")
       | v1, _ -> Error ("类型错误: * 的左操作数是 " ^ type_of_value v1 ^ "，需要整数"))
  
  | EDiv (e1, e2) ->
      let* (v1, _) = eval env e1 in
      let* (v2, _) = eval env e2 in
      (match v1, v2 with
       | VInt _, VInt 0 -> Error "除零错误"
       | VInt a, VInt b -> Ok (VInt (a / b), env)
       | VInt _, v2 -> Error ("类型错误: / 的右操作数是 " ^ type_of_value v2 ^ "，需要整数")
       | v1, _ -> Error ("类型错误: / 的左操作数是 " ^ type_of_value v1 ^ "，需要整数"))
  
  | EEq (e1, e2) ->
      let* (v1, _) = eval env e1 in
      let* (v2, _) = eval env e2 in
       (match v1, v2 with
        | VInt a, VInt b -> Ok (VBool (a = b), env)
        | VBool a, VBool b -> Ok (VBool (a = b), env)
        | VString a, VString b -> Ok (VBool (a = b), env)
        | VChar a, VChar b -> Ok (VBool (Char.equal a b), env)
        | VUnit, VUnit -> Ok (VBool true, env)
        | _, _ -> Ok (VBool false, env))
  
  | ENeq (e1, e2) ->
      let* (v1, _) = eval env e1 in
      let* (v2, _) = eval env e2 in
       (match v1, v2 with
        | VInt a, VInt b -> Ok (VBool (a <> b), env)
        | VBool a, VBool b -> Ok (VBool (a <> b), env)
        | VString a, VString b -> Ok (VBool (a <> b), env)
        | VChar a, VChar b -> Ok (VBool (not (Char.equal a b)), env)
        | VUnit, VUnit -> Ok (VBool false, env)
        | _, _ -> Ok (VBool true, env))
  
  | ELt (e1, e2) ->
      let* (v1, _) = eval env e1 in
      let* (v2, _) = eval env e2 in
      (match v1, v2 with
       | VInt a, VInt b -> Ok (VBool (a < b), env)
       | VString a, VString b -> Ok (VBool (a < b), env)
       | v1, v2 -> Error ("类型错误: < 的操作数是 " ^ type_of_value v1 ^ " 和 " ^ type_of_value v2 ^ "，需要整数或字符串"))
  
  | ELe (e1, e2) ->
      let* (v1, _) = eval env e1 in
      let* (v2, _) = eval env e2 in
      (match v1, v2 with
       | VInt a, VInt b -> Ok (VBool (a <= b), env)
       | VString a, VString b -> Ok (VBool (a <= b), env)
       | v1, v2 -> Error ("类型错误: <= 的操作数是 " ^ type_of_value v1 ^ " 和 " ^ type_of_value v2 ^ "，需要整数或字符串"))
  
  | EGt (e1, e2) ->
      let* (v1, _) = eval env e1 in
      let* (v2, _) = eval env e2 in
      (match v1, v2 with
       | VInt a, VInt b -> Ok (VBool (a > b), env)
       | VString a, VString b -> Ok (VBool (a > b), env)
       | v1, v2 -> Error ("类型错误: > 的操作数是 " ^ type_of_value v1 ^ " 和 " ^ type_of_value v2 ^ "，需要整数或字符串"))
  
  | EGe (e1, e2) ->
      let* (v1, _) = eval env e1 in
      let* (v2, _) = eval env e2 in
      (match v1, v2 with
       | VInt a, VInt b -> Ok (VBool (a >= b), env)
       | VString a, VString b -> Ok (VBool (a >= b), env)
       | v1, v2 -> Error ("类型错误: >= 的操作数是 " ^ type_of_value v1 ^ " 和 " ^ type_of_value v2 ^ "，需要整数或字符串"))
  
  | EAnd (e1, e2) ->
      let* (v1, _) = eval env e1 in
      (match v1 with
       | VBool true -> eval env e2
       | VBool false -> Ok (VBool false, env)
        | v -> Error ("类型错误: && 的操作数是 " ^ type_of_value v ^ "，需要布尔值"))
  
  | EOr (e1, e2) ->
      let* (v1, _) = eval env e1 in
      (match v1 with
       | VBool true -> Ok (VBool true, env)
       | VBool false -> eval env e2
       | v -> Error ("类型错误: || 的操作数是 " ^ type_of_value v ^ "，需要布尔值"))
  
  | ENot e ->
      let* (v, _) = eval env e in
      (match v with
       | VBool b -> Ok (VBool (not b), env)
       | v -> Error ("类型错误: not 的操作数是 " ^ type_of_value v ^ "，需要布尔值"))
  
  | EIf (cond, then_branch, else_branch) ->
      let* (v, _) = eval env cond in
      (match v with
       | VBool true -> eval env then_branch
       | VBool false -> eval env else_branch
       | v -> Error ("类型错误: if 的条件是 " ^ type_of_value v ^ "，需要布尔值"))
  
  | ELet (x, value_expr, body) ->
      let* (value, _) = eval env value_expr in
      eval ((x, value) :: env) body
  
  | ELetRec (f, value_expr, body) ->
      (match value_expr with
       | EFun (param, func_body) ->
           let rec env' = (f, VFun (Some f, param, func_body, env')) :: env in
           eval env' body
        | _ -> Error "let rec 后面必须是函数")
  
  | EFun (param, body) -> Ok (VFun (None, param, body, env), env)
  
  | EApp (func, arg) ->
      let* (func_val, _) = eval env func in
      let* (arg_val, _) = eval env arg in
      (match func_val with
       | VCtor (c, None) -> Ok (VCtor (c, Some arg_val), env)
       | _ -> apply_value env func_val arg_val)
  
  | ECat (e1, e2) ->
      let* (v1, _) = eval env e1 in
      let* (v2, _) = eval env e2 in
      (match v1, v2 with
       | VString a, VString b -> Ok (VString (a ^ b), env)
       | v1, v2 -> Error ("类型错误: ^ 的操作数是 " ^ type_of_value v1 ^ " 和 " ^ type_of_value v2 ^ "，需要字符串"))
  
  | ECons (e1, e2) ->
      let* (v1, _) = eval env e1 in
      let* (v2, _) = eval env e2 in
      (match v2 with
       | VList vs -> Ok (VList (v1 :: vs), env)
       | v -> Error ("类型错误: :: 的右边是 " ^ type_of_value v ^ "，需要列表"))
  
  | EMatch (e, cases) ->
      let* (v, _) = eval env e in
      Eval_pattern.eval_match eval env v cases

  | ESeq (e1, e2) ->
      let* (_, env') = eval env e1 in
      eval env' e2

  | EWhile (cond, body) ->
      let rec loop env =
        let* (v, _) = eval env cond in
        match v with
        | VBool true ->
            let* (_, env') = eval env body in
            loop env'
        | VBool false -> Ok (VUnit, env)
        | v -> Error ("类型错误: while 的条件是 " ^ type_of_value v ^ "，需要布尔值")
      in
      loop env

  | EIndex (e1, e2) ->
      let* (v1, _) = eval env e1 in
      let* (v2, _) = eval env e2 in
      (match v1, v2 with
       | VList vs, VInt idx ->
            (match Eval_helpers.list_nth_safe vs idx with
            | Some v -> Ok (v, env)
            | None -> Error ("索引越界: " ^ string_of_int idx))
        | VString s, VInt idx when idx >= 0 && idx < String.length s ->
            Ok (VString (String.make 1 s.[idx]), env)
        | VString _, VInt idx ->
            Error ("字符串索引越界: " ^ string_of_int idx)
        | v1, v2 -> Error ("类型错误: 索引的对象是 " ^ type_of_value v1 ^ "，索引值是 " ^ type_of_value v2 ^ "，需要列表/字符串和整数"))

  | ESlice (e, start, end_) ->
      let* (v, _) = eval env e in
      let* start_idx =
        match start with
        | Some s ->
            let* (sv, _) = eval env s in
            (match sv with
             | VInt n when n >= 0 -> Ok n
             | VInt n -> Error ("切片起始索引不能为负数: " ^ string_of_int n)
             | sv -> Error ("类型错误: 切片起始索引是 " ^ type_of_value sv ^ "，需要整数"))
        | None -> Ok 0
      in
      let* end_idx =
        match end_ with
        | Some e ->
            let* (ev, _) = eval env e in
            (match ev with
             | VInt n when n >= 0 -> Ok n
             | VInt n -> Error ("切片结束索引不能为负数: " ^ string_of_int n)
             | ev -> Error ("类型错误: 切片结束索引是 " ^ type_of_value ev ^ "，需要整数"))
        | None -> Ok (-1)
      in
      (match v with
       | VList vs ->
           let len = List.length vs in
           let real_start = min start_idx len in
           let real_end = if end_idx = -1 then len else min end_idx len in
           if real_start > real_end then Ok (VList [], env)
           else
             let rec take n = function
               | [] -> []
               | h :: t -> if n = 0 then [] else h :: take (n - 1) t
             in
             let rec drop n = function
               | [] -> []
               | h :: t -> if n = 0 then h :: t else drop (n - 1) t
             in
             Ok (VList (take (real_end - real_start) (drop real_start vs)), env)
       | VString s ->
           let len = String.length s in
           let real_start = min start_idx len in
           let real_end = if end_idx = -1 then len else min end_idx len in
           if real_start > real_end then Ok (VString "", env)
           else Ok (VString (String.sub s real_start (real_end - real_start)), env)
        | v -> Error ("类型错误: 切片的对象是 " ^ type_of_value v ^ "，需要列表或字符串"))

  | ECtor (c, None) -> Ok (VCtor (c, None), env)
  | ECtor (c, Some e) ->
      let* (v, _) = eval env e in
      Ok (VCtor (c, Some v), env)
  | ETypeDef _ -> Ok (VUnit, env)

  | ERef e ->
      let* (v, _) = eval env e in
      Ok (VRef (ref v), env)

  | EDeref e ->
      let* (v, _) = eval env e in
      (match v with
       | VRef r -> Ok (!r, env)
       | v -> Error ("类型错误: 解引用需要 ref，但得到 " ^ type_of_value v))

  | EAssign (e1, e2) ->
      (match e1 with
       | EArrayGet (arr, idx) ->
           let* (v1, _) = eval env arr in
           let* (v2, _) = eval env idx in
           let* (v3, _) = eval env e2 in
           (match v1, v2 with
            | VArray a, VInt i when i >= 0 && i < Array.length a ->
                Array.set a i v3; Ok (VUnit, env)
            | VArray _, VInt i ->
                Error ("数组索引越界: " ^ string_of_int i)
            | v1, v2 ->
                Error ("类型错误: 数组赋值需要 array 和 int，但得到 " ^ type_of_value v1 ^ " 和 " ^ type_of_value v2))
        | ERecordGet (e, field) | EDot (e, field) ->
            let* (v1, _) = eval env e in
            let* (v2, _) = eval env e2 in
            (match v1 with
             | VRecord fields ->
                 (match List.assoc_opt field fields with
                  | Some r ->
                      r := v2;
                      Ok (VUnit, env)
                  | None -> Error ("记录没有字段: " ^ field))
             | v -> Error ("类型错误: 字段赋值需要 record，但得到 " ^ type_of_value v))
        | _ ->
            let* (v1, _) = eval env e1 in
            let* (v2, _) = eval env e2 in
            (match v1 with
             | VRef r -> r := v2; Ok (VUnit, env)
             | v -> Error ("类型错误: 赋值需要 ref，但得到 " ^ type_of_value v)))

  | ERaise e ->
      let* (v, _) = eval env e in
      raise (Exception_value v)

  | ETry (e, cases) ->
      (try
         eval env e
       with
       | Exception_value v -> Eval_pattern.eval_match eval env v cases)

  | EAnnot (e, _) ->
      eval env e

  | ERange (start, end_) ->
      let* (v1, _) = eval env start in
      let* (v2, _) = eval env end_ in
      (match v1, v2 with
       | VInt s, VInt e when s <= e ->
           Ok (VList (List.init (e - s + 1) (fun i -> VInt (s + i))), env)
       | VInt s, VInt e when s > e ->
           Ok (VList [], env)
       | v1, v2 ->
           Error ("类型错误: 范围表达式需要整数，但得到 " ^ type_of_value v1 ^ " 和 " ^ type_of_value v2))

  | EArray es ->
      let* (vs, env') = eval_list env es in
      Ok (VArray (Array.of_list vs), env')

  | EArrayGet (arr, idx) ->
      let* (v1, _) = eval env arr in
      let* (v2, _) = eval env idx in
      (match v1, v2 with
       | VArray a, VInt i when i >= 0 && i < Array.length a ->
           Ok (Array.get a i, env)
       | VArray _, VInt i ->
           Error ("数组索引越界: " ^ string_of_int i)
        | v1, v2 ->
            Error ("类型错误: 数组索引需要 array 和 int，但得到 " ^ type_of_value v1 ^ " 和 " ^ type_of_value v2))

  | ERecord fields ->
      let* (vs, env') = eval_record_fields env fields in
      Ok (VRecord (List.map (fun (k, v) -> (k, ref v)) vs), env')

  | ERecordGet (e, field) ->
      let* (v, _) = eval env e in
      (match v with
       | VRecord fields ->
           (match List.assoc_opt field fields with
            | Some r -> Ok (!r, env)
            | None -> Error ("记录没有字段: " ^ field))
        | v -> Error ("类型错误: 字段访问需要 record，但得到 " ^ type_of_value v))

  | ERecordUpdate (e, fields) ->
      let* (v, _) = eval env e in
      (match v with
       | VRecord old_fields ->
           let* (new_vs, _) = eval_record_fields env fields in
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
           Ok (VRecord (merged @ added), env)
        | v -> Error ("类型错误: 记录更新需要 record，但得到 " ^ type_of_value v))

  | EModule (name, body) ->
      (* 求值模块体，收集导出的绑定 *)
      let module_env = ref [] in
      let rec extract_bindings env expr =
        match expr with
        | ELet (x, v, rest) ->
            let* (val_v, env') = eval env v in
            module_env := (x, val_v) :: !module_env;
            extract_bindings env' rest
        | ELetRec (x, v, rest) ->
            let* (val_v, env') = eval env v in
            module_env := (x, val_v) :: !module_env;
            extract_bindings env' rest
        | ETypeDef _ -> extract_bindings env body
        | ESeq (e1, e2) ->
            let* (_, env') = eval env e1 in
            extract_bindings env' e2
        | _ ->
            let* (v, _) = eval env expr in
            module_env := ("__value", v) :: !module_env;
            Ok ()
      in
      let* () = extract_bindings env body in
      let module_value = VModule (name, !module_env) in
      Ok (module_value, (name, module_value) :: env)

  | EModuleType (name, sig_expr) ->
      (* 模块类型签名：暂不实现完整签名检查 *)
      Ok (VUnit, env)

  | EOpen name ->
      (match List.assoc_opt name env with
       | Some (VModule (_, module_env)) ->
           (* 将模块的绑定导入到当前环境 *)
           Ok (VUnit, module_env @ env)
       | Some v -> Error ("open 需要模块，但得到 " ^ type_of_value v)
       | None -> Error ("未定义的模块: " ^ name))

  | EDot (e, field) ->
      let* (v, _) = eval env e in
      (match v with
       | VModule (_, module_env) ->
           (match List.assoc_opt field module_env with
            | Some fv -> Ok (fv, env)
            | None -> Error ("模块中未找到字段: " ^ field))
       | VRecord fields ->
           (match List.assoc_opt field fields with
            | Some r -> Ok (!r, env)
            | None -> Error ("记录没有字段: " ^ field))
       | VCtor (name, None) ->
           (* 构造函数可能被用作模块名，查找环境中的模块 *)
           (match List.assoc_opt name env with
            | Some (VModule (_, module_env)) ->
                (match List.assoc_opt field module_env with
                 | Some fv -> Ok (fv, env)
                 | None -> Error ("模块中未找到字段: " ^ field))
            | Some v -> Error ("点号访问需要模块或记录，但得到 " ^ type_of_value v)
            | None -> Error ("未定义的模块: " ^ name))
        | v -> Error ("点号访问需要模块或记录，但得到 " ^ type_of_value v))

  | ETraitDef (name, params, methods) ->
      let trait_def = {
        Traits.trait_name = name;
        type_params = params;
        methods = List.map (fun (mname, _) ->
          (mname, Types.TArrow (Types.TVar 0, Types.TVar 0))) methods;
      } in
      Traits.define_trait !trait_env trait_def;
      Ok (VUnit, env)

  | ETraitImpl (trait_name, type_name, methods) ->
      (* 1. 求值所有方法并存储到 trait_method_table *)
      let rec eval_methods env = function
        | [] -> Ok ()
        | (mname, mexpr) :: rest ->
            let* (mval, _) = eval env mexpr in
            let key = make_trait_key trait_name mname type_name in
            Hashtbl.replace trait_method_table key mval;
            eval_methods env rest
      in
      let* () = eval_methods env methods in
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
            | None -> Error ("未找到实现: " ^ trait_name ^ "." ^ mname ^ " for " ^ arg_type)
          ) in
          (mname, dispatch) :: env_acc
      ) env methods in
      Ok (VUnit, dispatch_env)

  | ESpawn e ->
      let* (v, _) = eval env e in
      (match v with
       | VFun _ | VBuiltin _ ->
           let f () =
             match apply_value env v VUnit with
             | Ok (result, _) -> result
             | Error msg -> VString ("Error: " ^ msg)
           in
           Ok (Actor.spawn_actor f, env)
       | _ -> Error "spawn 需要函数")

  | ESend (pid_e, msg_e) ->
      let* (pid_v, _) = eval env pid_e in
      let* (msg_v, _) = eval env msg_e in
      (match pid_v with
       | VInt pid ->
           Actor.send_message pid msg_v;
           Ok (VUnit, env)
        | _ -> Error "send 需要整数 pid")

  | EReceive ->
      let msg = Actor.receive_message () in
      Ok (msg, env)

  | EEffectDef (name, ops) ->
      (* 效果定义注册到环境，用于后续 perform 查找 *)
      let effect_env = List.fold_left (fun env_acc op ->
        (op, VBuiltin (op, fun env arg -> Error ("效果 " ^ op ^ " 未在 handle 中处理"))) :: env_acc
      ) env ops in
      Ok (VUnit, effect_env)

  | EPerform (op, arg) ->
      let* (v, _) = eval env arg in
      (match List.assoc_opt op env with
       | Some handler ->
           let resume_fn = VBuiltin ("resume", fun env arg -> Ok (arg, env)) in
           let* (partial1, _) = apply_value env handler v in
           let* (result, _) = apply_value env partial1 resume_fn in
           Ok (result, env)
       | None -> Error ("未处理的效果: " ^ op))

  | EHandle (e, handlers) ->
      (* 将 handler 转换为 curried 函数并添加到环境 *)
      let handler_env = List.fold_left (fun env_acc (op, arg_name, k_name, body) ->
        let handler_fn = VFun (None, arg_name, EFun (k_name, body), env_acc) in
        (op, handler_fn) :: env_acc
      ) env handlers in
      eval handler_env e

(** 求值列表表达式 *)
and eval_list env es =
  match es with
  | [] -> Ok ([], env)
  | e :: rest ->
      let* (v, env') = eval env e in
      let* (vs, env'') = eval_list env' rest in
      Ok (v :: vs, env'')

(** 求值记录字段 *)
and eval_record_fields env fields =
  match fields with
  | [] -> Ok ([], env)
  | (name, e) :: rest ->
      let* (v, env') = eval env e in
      let* (vs, env'') = eval_record_fields env' rest in
      Ok ((name, v) :: vs, env'')

(** 创建求值上下文 *)
let create_context () =
  { Eval_builtin.eval_fn = eval;
    Eval_builtin.apply_fn = apply_value }

(** 创建内置环境 *)
let builtin_env =
  let ctx = create_context () in
  Eval_builtin.create_builtin_env ctx

(** 内置类型环境 *)
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

(** 运行表达式 *)
let run expr =
  match eval builtin_env expr with
  | Ok (v, _) -> v
  | Error msg -> raise (RuntimeError (msg, None))

(** 运行表达式，返回 Result *)
let run_result env expr =
  match eval env expr with
  | Ok (v, _) -> Ok v
  | Error msg -> Error msg

(** 求值表达式，返回 Result *)
let eval_result expr = run_result builtin_env expr
