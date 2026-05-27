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
    | VInt n -> Ok (VString (string_of_int n), env)
    | v -> Error ("show: 需要 int，但得到 " ^ type_of_value v)) in
  Hashtbl.replace trait_method_table (make_trait_key "Show" "show" "int") int_show;
  let bool_show = VBuiltin ("show", fun env arg ->
    match arg with
    | VBool b -> Ok (VString (string_of_bool b), env)
    | v -> Error ("show: 需要 bool，但得到 " ^ type_of_value v)) in
  Hashtbl.replace trait_method_table (make_trait_key "Show" "show" "bool") bool_show;
  let int_eq = VBuiltin ("eq", fun env arg ->
    Ok (VBuiltin ("eq'", fun env arg2 ->
      match arg, arg2 with
      | VInt a, VInt b -> Ok (VBool (a = b), env)
      | v1, v2 -> Error ("eq: 需要两个 int，但得到 " ^ type_of_value v1 ^ " 和 " ^ type_of_value v2)),
     env)) in
  Hashtbl.replace trait_method_table (make_trait_key "Eq" "eq" "int") int_eq;
  let int_neq = VBuiltin ("neq", fun env arg ->
    Ok (VBuiltin ("neq'", fun env arg2 ->
      match arg, arg2 with
      | VInt a, VInt b -> Ok (VBool (a <> b), env)
      | v1, v2 -> Error ("neq: 需要两个 int，但得到 " ^ type_of_value v1 ^ " 和 " ^ type_of_value v2)),
     env)) in
  Hashtbl.replace trait_method_table (make_trait_key "Eq" "neq" "int") int_neq

let lookup env x =
  match List.assoc_opt x env with
  | Some v -> v
  | None -> raise (RuntimeError ("未绑定变量: " ^ x, None))

let ( let* ) = Result.bind

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
  | EVar x -> Ok (lookup env x, env)
  
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
      eval_match env v cases

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
           (match list_nth_safe vs idx with
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
       | Exception_value v -> eval_match env v cases)

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

  and eval_list env es =
  match es with
  | [] -> Ok ([], env)
  | e :: rest ->
      let* (v, env') = eval env e in
      let* (vs, env'') = eval_list env' rest in
      Ok (v :: vs, env'')

and eval_record_fields env fields =
  match fields with
  | [] -> Ok ([], env)
  | (name, e) :: rest ->
      let* (v, env') = eval env e in
      let* (vs, env'') = eval_record_fields env' rest in
      Ok ((name, v) :: vs, env'')

and eval_match env v cases =
  match cases with
  | [] -> Error "匹配失败: 没有匹配的模式"
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
        (try
           let content = Core.In_channel.read_all filename in
           let lexbuf = Lexing.from_string content in
           let expr = Parser.prog Lexer.read lexbuf in
           let* (_, env') = eval env expr in
           Ok (VUnit, env')
         with Sys_error msg -> Error ("无法导入文件: " ^ msg))
    | _ -> Error "import: 需要字符串文件名"
  in
  [ ( "head",
      VBuiltin
        ( "head",
          fun env -> function
          | VList (h :: _) -> Ok (h, env)
          | VList [] -> Error "head: 空列表"
          | _ -> Error "head: 需要列表" ) )
  ; ( "tail",
      VBuiltin
        ( "tail",
          fun env -> function
          | VList (_ :: t) -> Ok (VList t, env)
          | VList [] -> Error "tail: 空列表"
          | _ -> Error "tail: 需要列表" ) )
  ; ( "length",
      VBuiltin
        ( "length",
          fun env -> function
          | VList l -> Ok (VInt (List.length l), env)
          | VString s -> Ok (VInt (String.length s), env)
          | _ -> Error "length: 需要列表或字符串" ) )
  ; ( "print",
      VBuiltin
        ( "print",
          fun env v ->
            print_endline (string_of_value v);
            Ok (VUnit, env) ) )
  ; ( "import",
      VBuiltin
        ( "import",
          import_func ) )
  ; ( "show",
      VBuiltin
        ( "show",
          fun env v ->
            Ok (VString (string_of_value v), env) ) )
  ; ( "string_length",
      VBuiltin
        ( "string_length",
          fun env -> function
          | VString s -> Ok (VInt (String.length s), env)
          | v -> Error ("string_length: 需要字符串，但得到 " ^ type_of_value v) ) )
  ; ( "string_get",
      VBuiltin
        ( "string_get",
          fun env s ->
            Ok (VBuiltin
               ( "string_get'",
                 fun env idx ->
                   match s, idx with
                   | VString s, VInt i when i >= 0 && i < String.length s ->
                       Ok (VChar s.[i], env)
                   | VString _, VInt i ->
                       Error ("string_get: 索引越界: " ^ string_of_int i)
                   | VString _, v ->
                       Error ("string_get: 索引需要整数，但得到 " ^ type_of_value v)
                   | v, _ ->
                       Error ("string_get: 需要字符串，但得到 " ^ type_of_value v) ),
             env) ) )
  ; ( "string_sub",
      VBuiltin
        ( "string_sub",
          fun env s ->
            Ok (VBuiltin
               ( "string_sub'",
                 fun env start ->
                   Ok (VBuiltin
                      ( "string_sub''",
                        fun env len ->
                          match s, start, len with
                          | VString s, VInt start, VInt len when start >= 0 && len >= 0 && start + len <= String.length s ->
                              Ok (VString (String.sub s start len), env)
                          | VString _, VInt _, VInt _ ->
                              Error "string_sub: 索引越界"
                          | VString _, VInt _, v ->
                              Error ("string_sub: 长度需要整数，但得到 " ^ type_of_value v)
                          | VString _, v, _ ->
                              Error ("string_sub: 起始需要整数，但得到 " ^ type_of_value v)
                          | v, _, _ ->
                              Error ("string_sub: 需要字符串，但得到 " ^ type_of_value v) ),
                    env) ),
             env) ) )
  ; ( "read_file",
      VBuiltin
        ( "read_file",
          fun env -> function
          | VString filename ->
              (try
                 let content = Core.In_channel.read_all filename in
                 Ok (VString content, env)
               with Sys_error msg -> Error ("无法读取文件: " ^ msg))
          | v -> Error ("read_file: 需要字符串文件名，但得到 " ^ type_of_value v) ) )
  ; ( "write_file",
      VBuiltin
        ( "write_file",
          fun env filename ->
            Ok (VBuiltin
               ( "write_file'",
                 fun env content ->
                   match filename, content with
                   | VString filename, VString content ->
                       (try
                          Core.Out_channel.write_all filename ~data:content;
                          Ok (VUnit, env)
                        with Sys_error msg -> Error ("无法写入文件: " ^ msg))
                   | VString _, v ->
                       Error ("write_file: 内容需要字符串，但得到 " ^ type_of_value v)
                   | v, _ ->
                       Error ("write_file: 文件名需要字符串，但得到 " ^ type_of_value v) ),
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
              Ok (VString line, env)
          | v -> Error ("read_line: 需要 unit，但得到 " ^ type_of_value v) ) )
  ; ( "print_string",
      VBuiltin
        ( "print_string",
          fun env -> function
          | VString s ->
              print_string s;
              Ok (VUnit, env)
          | v -> Error ("print_string: 需要字符串，但得到 " ^ type_of_value v) ) )
  ; ( "map",
      VBuiltin
        ( "map",
          fun env f ->
            Ok (VBuiltin
               ( "map'",
                 fun env xs ->
                   match xs with
                   | VList items ->
                       let rec map_items = function
                         | [] -> Ok []
                         | item :: rest ->
                             let* (v, _) = apply_value env f item in
                             let* vs' = map_items rest in
                             Ok (v :: vs')
                       in
                       let* results = map_items items in
                       Ok (VList results, env)
                   | v -> Error ("map: 第二个参数必须是列表，但得到 " ^ type_of_value v) ),
             env)
        ) )
  ; ( "filter",
      VBuiltin
        ( "filter",
          fun env f ->
            Ok (VBuiltin
               ( "filter'",
                 fun env xs ->
                   match xs with
                   | VList items ->
                       let rec filter_items = function
                         | [] -> Ok []
                         | item :: rest ->
                             let* (v, _) = apply_value env f item in
                             let* rest' = filter_items rest in
                             (match v with
                              | VBool b -> Ok (if b then item :: rest' else rest')
                              | v -> Error ("filter: 谓词函数必须返回布尔值，但得到 " ^ type_of_value v))
                       in
                       let* results = filter_items items in
                       Ok (VList results, env)
                   | v -> Error ("filter: 第二个参数必须是列表，但得到 " ^ type_of_value v) ),
             env)
        ) )
  ; ( "fold",
      VBuiltin
        ( "fold",
          fun env f ->
            Ok (VBuiltin
               ( "fold'",
                 fun env acc ->
                   Ok (VBuiltin
                      ( "fold''",
                        fun env xs ->
                          match xs with
                          | VList items ->
                              let rec fold_items acc = function
                                | [] -> Ok acc
                                | item :: rest ->
                                    let* (f_acc, _) = apply_value env f acc in
                                    (match f_acc with
                                     | VFun _ | VBuiltin _ ->
                                         let* (v, _) = apply_value env f_acc item in
                                         fold_items v rest
                                     | v -> Error ("fold: folding 函数必须接受两个参数，但得到 " ^ type_of_value v))
                              in
                              let* result = fold_items acc items in
                              Ok (result, env)
                           | v -> Error ("fold: 第三个参数必须是列表，但得到 " ^ type_of_value v) ),
                      env) ),
             env) ) )
  ; ( "range",
      VBuiltin
        ( "range",
          fun env start ->
            Ok (VBuiltin
               ( "range'",
                 fun env end_val ->
                   match start, end_val with
                   | VInt s, VInt e ->
                       let rec build_range i acc =
                         if i > e then List.rev acc
                         else build_range (i + 1) (VInt i :: acc)
                       in
                       let nums = build_range s [] in
                       Ok (VList nums, env)
                   | _ -> Error "range: 需要整数参数" ),
             env) ) )
  ; ( "sum",
      VBuiltin
        ( "sum",
          fun env xs ->
            match xs with
            | VList items ->
                let rec sum_items acc = function
                  | [] -> Ok acc
                  | VInt n :: rest -> sum_items (acc + n) rest
                  | _ -> Error "sum: 列表元素必须是整数"
                in
                let* total = sum_items 0 items in
                Ok (VInt total, env)
            | _ -> Error "sum: 需要列表" ) )
  ; ( "reverse",
      VBuiltin
        ( "reverse",
          fun env xs ->
            match xs with
            | VList items -> Ok (VList (List.rev items), env)
            | _ -> Error "reverse: 需要列表" ) )
  ; ( "append",
      VBuiltin
        ( "append",
          fun env xs ->
            Ok (VBuiltin
               ( "append'",
                 fun env ys ->
                   match xs, ys with
                   | VList a, VList b -> Ok (VList (a @ b), env)
                   | _ -> Error "append: 需要两个列表" ),
             env) ) )
  ; ( "timeit",
      VBuiltin
        ( "timeit",
          fun env f ->
            match f with
            | VFun _ | VBuiltin _ ->
                let start = Core.Time_float.now () in
                let* (result, _) = apply_value env f (VTuple []) in
                let elapsed = Core.Time_float.diff (Core.Time_float.now ()) start in
                let ms = Core.Time_float.Span.to_ms elapsed in
                Printf.printf "[timeit] %.4f ms\n%!" ms;
                Ok (result, env)
            | _ -> Error "timeit: 需要函数" ) )
  ; ( "string_trim",
      VBuiltin
        ( "string_trim",
          fun env -> function
          | VString s -> Ok (VString (String.trim s), env)
          | v -> Error ("string_trim: 需要字符串，但得到 " ^ type_of_value v) ) )
  ; ( "string_uppercase",
      VBuiltin
        ( "string_uppercase",
          fun env -> function
          | VString s -> Ok (VString (String.uppercase_ascii s), env)
          | v -> Error ("string_uppercase: 需要字符串，但得到 " ^ type_of_value v) ) )
  ; ( "string_lowercase",
      VBuiltin
        ( "string_lowercase",
          fun env -> function
          | VString s -> Ok (VString (String.lowercase_ascii s), env)
          | v -> Error ("string_lowercase: 需要字符串，但得到 " ^ type_of_value v) ) )
  ; ( "string_concat",
      VBuiltin
        ( "string_concat",
          fun env -> function
          | VTuple [VString sep; VList items] ->
              let rec extract_strings = function
                | [] -> Ok []
                | VString s :: rest ->
                    let* rest' = extract_strings rest in
                    Ok (s :: rest')
                | _ -> Error "string_concat: 列表元素必须是字符串"
              in
              let* strs = extract_strings items in
              Ok (VString (String.concat sep strs), env)
          | v -> Error ("string_concat: 需要 (分隔符, 字符串列表) 元组，但得到 " ^ type_of_value v) ) )
  ; ( "string_split",
      VBuiltin
        ( "string_split",
          fun env -> function
          | VTuple [VString sep; VString s] ->
              let parts = Core.String.split s ~on:(sep.[0]) in
              Ok (VList (List.map (fun p -> VString p) parts), env)
          | v -> Error ("string_split: 需要 (分隔符, 字符串) 元组，但得到 " ^ type_of_value v) ) )
  ; ( "string_contains",
      VBuiltin
        ( "string_contains",
          fun env -> function
          | VTuple [VString substr; VString s] -> Ok (VBool (Core.String.is_substring s ~substring:substr), env)
          | v -> Error ("string_contains: 需要 (子串, 字符串) 元组，但得到 " ^ type_of_value v) ) )
  ; ( "string_replace",
      VBuiltin
        ( "string_replace",
          fun env -> function
          | VTuple [VString old_s; VString new_s; VString s] -> Ok (VString (Core.String.substr_replace_all s ~pattern:old_s ~with_:new_s), env)
          | v -> Error ("string_replace: 需要 (旧字符串, 新字符串, 字符串) 元组，但得到 " ^ type_of_value v) ) )
  ; ( "take",
      VBuiltin
        ( "take",
          fun env -> function
          | VTuple [VInt n; VList items] when n >= 0 -> Ok (VList (Core.List.take items n), env)
          | VTuple [VInt n; VList _] when n < 0 -> Error "take: 参数不能为负数"
          | v -> Error ("take: 需要 (整数, 列表) 元组，但得到 " ^ type_of_value v) ) )
  ; ( "drop",
      VBuiltin
        ( "drop",
          fun env -> function
          | VTuple [VInt n; VList items] when n >= 0 -> Ok (VList (Core.List.drop items n), env)
          | VTuple [VInt n; VList _] when n < 0 -> Error "drop: 参数不能为负数"
          | v -> Error ("drop: 需要 (整数, 列表) 元组，但得到 " ^ type_of_value v) ) )
  ; ( "find",
      VBuiltin
        ( "find",
          fun env -> function
          | VTuple [f; VList items] ->
              let rec find_loop = function
                | [] -> Ok (VCtor ("None", None), env)
                | h :: t ->
                    let* (v, _) = apply_value env f h in
                    (match v with
                     | VBool true -> Ok (VCtor ("Some", Some h), env)
                     | VBool false -> find_loop t
                     | _ -> Error "find: 谓词函数必须返回布尔值")
              in
              find_loop items
          | v -> Error ("find: 需要 (函数, 列表) 元组，但得到 " ^ type_of_value v) ) )
  ; ( "exists",
      VBuiltin
        ( "exists",
          fun env -> function
          | VTuple [f; VList items] ->
              let rec exists_loop = function
                | [] -> Ok (VBool false, env)
                | h :: t ->
                    let* (v, _) = apply_value env f h in
                    (match v with
                     | VBool true -> Ok (VBool true, env)
                     | VBool false -> exists_loop t
                     | _ -> Error "exists: 谓词函数必须返回布尔值")
              in
              exists_loop items
          | v -> Error ("exists: 需要 (函数, 列表) 元组，但得到 " ^ type_of_value v) ) )
  ; ( "forall",
      VBuiltin
        ( "forall",
          fun env -> function
          | VTuple [f; VList items] ->
              let rec forall_loop = function
                | [] -> Ok (VBool true, env)
                | h :: t ->
                    let* (v, _) = apply_value env f h in
                    (match v with
                     | VBool true -> forall_loop t
                     | VBool false -> Ok (VBool false, env)
                     | _ -> Error "forall: 谓词函数必须返回布尔值")
              in
              forall_loop items
          | v -> Error ("forall: 需要 (函数, 列表) 元组，但得到 " ^ type_of_value v) ) )
  ; ( "sort",
      VBuiltin
        ( "sort",
          fun env -> function
          | VList items ->
              let sorted = Core.List.sort ~compare:(fun a b -> match a, b with VInt x, VInt y -> Int.compare x y | VString x, VString y -> String.compare x y | _ -> 0) items in
              Ok (VList sorted, env)
          | v -> Error ("sort: 需要列表，但得到 " ^ type_of_value v) ) )
  ; ( "zip",
      VBuiltin
        ( "zip",
          fun env -> function
          | VTuple [VList a; VList b] ->
              let zipped = Core.List.map2_exn ~f:(fun x y -> VTuple [x; y]) a b in
              Ok (VList zipped, env)
          | v -> Error ("zip: 需要 (列表, 列表) 元组，但得到 " ^ type_of_value v) ) )
  ; ( "abs",
      VBuiltin
        ( "abs",
          fun env -> function
          | VInt n -> Ok (VInt (Int.abs n), env)
          | v -> Error ("abs: 需要整数，但得到 " ^ type_of_value v) ) )
  ; ( "min",
      VBuiltin
        ( "min",
          fun env -> function
          | VTuple [VInt x; VInt y] -> Ok (VInt (Int.min x y), env)
          | VTuple [VString x; VString y] -> Ok (VString (if String.compare x y <= 0 then x else y), env)
          | v -> Error ("min: 需要两个整数或两个字符串的元组，但得到 " ^ type_of_value v) ) )
  ; ( "max",
      VBuiltin
        ( "max",
          fun env -> function
          | VTuple [VInt x; VInt y] -> Ok (VInt (Int.max x y), env)
          | VTuple [VString x; VString y] -> Ok (VString (if String.compare x y >= 0 then x else y), env)
          | v -> Error ("max: 需要两个整数或两个字符串的元组，但得到 " ^ type_of_value v) ) )
  ; ( "int_of_string",
      VBuiltin
        ( "int_of_string",
          fun env -> function
          | VString s -> (try Ok (VInt (int_of_string s), env) with Failure _ -> Error ("int_of_string: 无效的整数字符串: " ^ s))
          | v -> Error ("int_of_string: 需要字符串，但得到 " ^ type_of_value v) ) )
  ; ( "string_of_int",
      VBuiltin
        ( "string_of_int",
          fun env -> function
          | VInt n -> Ok (VString (Int.to_string n), env)
          | v -> Error ("string_of_int: 需要整数，但得到 " ^ type_of_value v) ) )
  ; ( "int_of_char",
      VBuiltin
        ( "int_of_char",
          fun env -> function
          | VChar c -> Ok (VInt (Char.code c), env)
          | v -> Error ("int_of_char: 需要字符，但得到 " ^ type_of_value v) ) )
   ; ( "char_of_int",
      VBuiltin
        ( "char_of_int",
          fun env -> function
          | VInt n -> if n >= 0 && n <= 255 then Ok (VChar (Char.chr n), env) else Error "char_of_int: 超出字符范围 (0-255)"
          | v -> Error ("char_of_int: 需要整数，但得到 " ^ type_of_value v) ) )
   ; ( "sqrt",
      VBuiltin
        ( "sqrt",
          fun env -> function
          | VInt n -> if n >= 0 then Ok (VInt (int_of_float (sqrt (float_of_int n))), env) else Error "sqrt: 不能对负数开方"
          | v -> Error ("sqrt: 需要整数，但得到 " ^ type_of_value v) ) )
   ; ( "pow",
      VBuiltin
        ( "pow",
          fun env -> function
          | VTuple [VInt base; VInt exp] -> Ok (VInt (int_of_float ((float_of_int base) ** (float_of_int exp))), env)
          | v -> Error ("pow: 需要两个整数，但得到 " ^ type_of_value v) ) )
   ; ( "random_int",
      VBuiltin
        ( "random_int",
          fun env -> function
          | VTuple [VInt min; VInt max] ->
              if min <= max then Ok (VInt (min + Random.int (max - min + 1)), env)
              else Error "random_int: 最小值不能大于最大值"
          | v -> Error ("random_int: 需要两个整数，但得到 " ^ type_of_value v) ) )
   ; ( "current_time",
      VBuiltin
        ( "current_time",
          fun env -> function
          | VTuple [] -> Ok (VInt (int_of_float (Unix.gettimeofday ())), env)
          | VUnit -> Ok (VInt (int_of_float (Unix.gettimeofday ())), env)
          | v -> Error ("current_time: 需要 unit，但得到 " ^ type_of_value v) ) )
   ; ( "sleep",
      VBuiltin
        ( "sleep",
          fun env -> function
          | VInt ms -> (Unix.sleepf (float_of_int ms /. 1000.0); Ok (VUnit, env))
          | v -> Error ("sleep: 需要整数（毫秒），但得到 " ^ type_of_value v) ) )
   ; ( "file_exists",
      VBuiltin
        ( "file_exists",
          fun env -> function
          | VString path -> Ok (VBool (Stdlib.Sys.file_exists path), env)
          | v -> Error ("file_exists: 需要字符串，但得到 " ^ type_of_value v) ) )
   ; ( "file_size",
      VBuiltin
        ( "file_size",
          fun env -> function
          | VString path ->
              (try
                 let ic = Stdlib.open_in path in
                 let size = Stdlib.in_channel_length ic in
                 Stdlib.close_in ic;
                 Ok (VInt size, env)
               with _ -> Error ("file_size: 无法获取文件大小: " ^ path))
          | v -> Error ("file_size: 需要字符串，但得到 " ^ type_of_value v) ) )
   ; ( "delete_file",
      VBuiltin
        ( "delete_file",
          fun env -> function
          | VString path -> (Stdlib.Sys.remove path; Ok (VUnit, env))
          | v -> Error ("delete_file: 需要字符串，但得到 " ^ type_of_value v) ) )
   ; ( "list_directory",
      VBuiltin
        ( "list_directory",
          fun env -> function
          | VString path ->
              (try
                 let files = Stdlib.Sys.readdir path |> Array.to_list in
                 Ok (VList (List.map (fun f -> VString f) files), env)
               with _ -> Error ("list_directory: 无法读取目录: " ^ path))
          | v -> Error ("list_directory: 需要字符串，但得到 " ^ type_of_value v) ) )
   ; ( "get_env",
      VBuiltin
        ( "get_env",
          fun env -> function
          | VString var ->
              (try
                 let value = Stdlib.Sys.getenv var in
                 Ok (VCtor ("Some", Some (VString value)), env)
               with Not_found -> Ok (VCtor ("None", None), env))
          | v -> Error ("get_env: 需要字符串，但得到 " ^ type_of_value v) ) )
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
              Ok (VInt code, env)
          | v -> Error ("system_command: 需要字符串，但得到 " ^ type_of_value v) ) )
     ; ( "regex_match",
       VBuiltin
         ( "regex_match",
           fun env -> function
           | VTuple [VString pattern; VString text] ->
               (try
                  let re = Str.regexp pattern in
                  Ok (VBool (Str.string_match re text 0), env)
                with _ -> Error ("regex_match: 无效的正则表达式: " ^ pattern))
           | v -> Error ("regex_match: 需要(模式, 文本)，但得到 " ^ type_of_value v) ) )
    ; ( "regex_replace",
       VBuiltin
         ( "regex_replace",
           fun env -> function
           | VTuple [VString pattern; VString replacement; VString text] ->
               (try
                  let re = Str.regexp pattern in
                  Ok (VString (Str.global_replace re replacement text), env)
                with _ -> Error ("regex_replace: 无效的正则表达式: " ^ pattern))
           | v -> Error ("regex_replace: 需要(模式, 替换, 文本)，但得到 " ^ type_of_value v) ) )
    ; ( "regex_split",
       VBuiltin
         ( "regex_split",
           fun env -> function
           | VTuple [VString pattern; VString text] ->
               (try
                  let re = Str.regexp pattern in
                  let parts = Str.split re text in
                  Ok (VList (List.map (fun s -> VString s) parts), env)
                with _ -> Error ("regex_split: 无效的正则表达式: " ^ pattern))
           | v -> Error ("regex_split: 需要(模式, 文本)，但得到 " ^ type_of_value v) ) )
    ]

let run expr =
  match eval builtin_env expr with
  | Ok (v, _) -> v
  | Error msg -> raise (RuntimeError (msg, None))

let run_result env expr =
  match eval env expr with
  | Ok (v, _) -> Ok v
  | Error msg -> Error msg

let eval_result expr = run_result builtin_env expr
