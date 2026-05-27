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
  match List.assoc_opt x env with
  | Some v -> Ok v
  | None -> Error ("未绑定变量: " ^ x)

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
