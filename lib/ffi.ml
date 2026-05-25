(** FFI (Foreign Function Interface) 外部函数接口

    允许 MyLang 调用 C 函数，实现与现有生态的互操作。
    
    语法：
    ```
    foreign sin : float -> float
    let x = sin 3.14
    ```
*)

open Core

(** C 类型映射 *)
type c_type =
  | CInt
  | CFloat
  | CBool
  | CString
  | CVoid
  | CPointer

(** FFI 函数声明 *)
type ffi_decl = {
  name : string;
  c_name : string;      (* C 函数名 *)
  params : c_type list;
  ret : c_type;
}

(** FFI 环境 *)
type ffi_env = {
  mutable decls : (string, ffi_decl) Hashtbl.t;
}

let create_ffi_env () = {
  decls = Hashtbl.create (module String);
}

(** 注册 FFI 声明 *)
let declare env decl =
  Hashtbl.set env.decls ~key:decl.name ~data:decl

(** 查找 FFI 声明 *)
let lookup env name =
  Hashtbl.find env.decls name

(** 内置数学库 FFI *)
let math_ffi () =
  let env = create_ffi_env () in
  declare env {
    name = "sin";
    c_name = "sin";
    params = [CFloat];
    ret = CFloat;
  };
  declare env {
    name = "cos";
    c_name = "cos";
    params = [CFloat];
    ret = CFloat;
  };
  declare env {
    name = "sqrt";
    c_name = "sqrt";
    params = [CFloat];
    ret = CFloat;
  };
  declare env {
    name = "pow";
    c_name = "pow";
    params = [CFloat; CFloat];
    ret = CFloat;
  };
  env

(** 内置字符串 FFI *)
let string_ffi () =
  let env = create_ffi_env () in
  declare env {
    name = "strlen";
    c_name = "strlen";
    params = [CString];
    ret = CInt;
  };
  declare env {
    name = "strcmp";
    c_name = "strcmp";
    params = [CString; CString];
    ret = CInt;
  };
  env

(** C 类型到 MyLang 类型 *)
let c_type_to_string = function
  | CInt -> "int"
  | CFloat -> "float"
  | CBool -> "bool"
  | CString -> "string"
  | CVoid -> "unit"
  | CPointer -> "pointer"

(** 生成 C 头文件 *)
let generate_header env =
  let decls = Hashtbl.data env.decls in
  let lines = List.map decls ~f:(fun decl ->
    let params_str = String.concat ~sep:", " (List.map decl.params ~f:c_type_to_string) in
    Printf.sprintf "extern %s %s(%s);" (c_type_to_string decl.ret) decl.c_name params_str) in
  String.concat ~sep:"\n" lines

(** FFI 调用记录（用于运行时） *)
type ffi_call = {
  call_name : string;
  call_args : Reg_bytecode.reg_value list;
}

(** 模拟 FFI 调用（纯 OCaml 实现，无实际 C 调用） *)
let simulate_call decl args =
  match decl.name, args with
  | "sin", [Reg_bytecode.RVInt n] -> Reg_bytecode.RVInt (int_of_float (Float.sin (float_of_int n)))
  | "cos", [Reg_bytecode.RVInt n] -> Reg_bytecode.RVInt (int_of_float (Float.cos (float_of_int n)))
  | "sqrt", [Reg_bytecode.RVInt n] -> Reg_bytecode.RVInt (int_of_float (Float.sqrt (float_of_int n)))
  | "pow", [Reg_bytecode.RVInt a; Reg_bytecode.RVInt b] ->
      Reg_bytecode.RVInt (int_of_float ((float_of_int a) ** (float_of_int b)))
  | "strlen", [Reg_bytecode.RVString s] -> Reg_bytecode.RVInt (String.length s)
  | "strcmp", [Reg_bytecode.RVString a; Reg_bytecode.RVString b] ->
      Reg_bytecode.RVInt (String.compare a b)
  | _ -> Reg_bytecode.RVInt 0

(** 解析 FFI 声明字符串 *)
let parse_decl s =
  (* 简化解析: "foreign name : param1 -> param2 -> ret" *)
  let parts = String.split s ~on:':' in
  match parts with
  | [name_part; type_part] ->
      let name = String.strip name_part in
      let types = String.split type_part ~on:'-' in
      let types = List.filter types ~f:(fun s -> not (String.is_empty s)) in
      let types = List.map types ~f:String.strip in
      let c_types = List.map types ~f:(fun t ->
        match t with
        | "int" -> CInt
        | "float" -> CFloat
        | "bool" -> CBool
        | "string" -> CString
        | "unit" -> CVoid
        | _ -> CPointer) in
      (match List.rev c_types with
       | ret :: params_rev ->
           let params = List.rev params_rev in
           Some { name; c_name = name; params; ret }
       | [] -> None)
  | _ -> None
