(** 求值器辅助函数和类型 *)

open Ast

(** 安全的列表索引，避免两次遍历 *)
let list_nth_safe lst idx =
  let rec loop i = function
    | [] -> None
    | x :: _ when i = idx -> Some x
    | _ :: xs -> loop (i + 1) xs
  in
  if idx < 0 then None else loop 0 lst

(** 运行时异常 *)
exception RuntimeError of string * pos option

(** 用户抛出的异常值 *)
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

(** Result monad 绑定操作符 *)
let ( let* ) = Result.bind

(** 从环境中查找变量 *)
let lookup env x =
  Env.lookup env x

(** 整数二元运算辅助函数 *)
let eval_binop_int eval_fn env e1 e2 op op_name =
  let* (v1, _) = eval_fn env e1 in
  let* (v2, _) = eval_fn env e2 in
  match v1, v2 with
  | VInt a, VInt b -> Ok (VInt (op a b), env)
  | VInt _, v2 -> Error (op_name ^ ": 右操作数需要整数，但得到 " ^ type_of_value v2)
  | v1, _ -> Error (op_name ^ ": 左操作数需要整数，但得到 " ^ type_of_value v1)

(** 整数除法辅助函数 *)
let eval_div eval_fn env e1 e2 =
  let* (v1, _) = eval_fn env e1 in
  let* (v2, _) = eval_fn env e2 in
  match v1, v2 with
  | VInt _, VInt 0 -> Error "除零错误"
  | VInt a, VInt b -> Ok (VInt (a / b), env)
  | VInt _, v2 -> Error ("/: 右操作数需要整数，但得到 " ^ type_of_value v2)
  | v1, _ -> Error ("/: 左操作数需要整数，但得到 " ^ type_of_value v1)

(** 比较运算辅助函数（整数） *)
let eval_compare_int eval_fn env e1 e2 op op_name =
  let* (v1, _) = eval_fn env e1 in
  let* (v2, _) = eval_fn env e2 in
  match v1, v2 with
  | VInt a, VInt b -> Ok (VBool (op a b), env)
  | v1, v2 -> Error (op_name ^ ": 操作数需要整数，但得到 " ^ type_of_value v1 ^ " 和 " ^ type_of_value v2)

(** 比较运算辅助函数（整数和字符串） *)
let eval_compare eval_fn env e1 e2 int_op string_op op_name =
  let* (v1, _) = eval_fn env e1 in
  let* (v2, _) = eval_fn env e2 in
  match v1, v2 with
  | VInt a, VInt b -> Ok (VBool (int_op a b), env)
  | VString a, VString b -> Ok (VBool (string_op a b), env)
  | v1, v2 -> Error (op_name ^ ": 操作数需要整数或字符串，但得到 " ^ type_of_value v1 ^ " 和 " ^ type_of_value v2)

(** 相等比较辅助函数 *)
let eval_equality eval_fn env e1 e2 =
  let* (v1, _) = eval_fn env e1 in
  let* (v2, _) = eval_fn env e2 in
  match v1, v2 with
  | VInt a, VInt b -> Ok (VBool (a = b), env)
  | VBool a, VBool b -> Ok (VBool (a = b), env)
  | VString a, VString b -> Ok (VBool (a = b), env)
  | VChar a, VChar b -> Ok (VBool (Char.equal a b), env)
  | VUnit, VUnit -> Ok (VBool true, env)
  | _, _ -> Ok (VBool false, env)

(** 不等比较辅助函数 *)
let eval_inequality eval_fn env e1 e2 =
  let* (v1, _) = eval_fn env e1 in
  let* (v2, _) = eval_fn env e2 in
  match v1, v2 with
  | VInt a, VInt b -> Ok (VBool (a <> b), env)
  | VBool a, VBool b -> Ok (VBool (a <> b), env)
  | VString a, VString b -> Ok (VBool (a <> b), env)
  | VChar a, VChar b -> Ok (VBool (not (Char.equal a b)), env)
  | VUnit, VUnit -> Ok (VBool false, env)
  | _, _ -> Ok (VBool true, env)

(** 字符串拼接辅助函数 *)
let eval_concat eval_fn env e1 e2 =
  let* (v1, _) = eval_fn env e1 in
  let* (v2, _) = eval_fn env e2 in
  match v1, v2 with
  | VString a, VString b -> Ok (VString (a ^ b), env)
  | v1, v2 -> Error ("^: 操作数需要字符串，但得到 " ^ type_of_value v1 ^ " 和 " ^ type_of_value v2)

(** 注册内置 trait 实现 *)
let init_traits () =
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
