(** FFI (Foreign Function Interface) 模块

    通过 Chez Scheme 的 foreign-procedure 机制，
    让 MyLang 具备近乎零开销调用原生 C 库的能力。
*)

open Ast

(** FFI 函数声明 *)
type ffi_decl = {
  name : string;              (* MyLang 中的函数名 *)
  c_name : string;            (* C 函数名 *)
  return_type : ffi_type;     (* 返回类型 *)
  arg_types : ffi_type list;  (* 参数类型 *)
  library : string option;    (* 可选的库名 *)
}

(** FFI 类型 *)
and ffi_type =
  | FFI_int
  | FFI_float
  | FFI_double
  | FFI_string
  | FFI_bool
  | FFI_void
  | FFI_pointer

(** 将 FFI 类型转换为 Chez Scheme 类型字符串 *)
let string_of_ffi_type = function
  | FFI_int -> "int"
  | FFI_float -> "float"
  | FFI_double -> "double"
  | FFI_string -> "string"
  | FFI_bool -> "int"  (* bool 作为 int 传递 *)
  | FFI_void -> "void"
  | FFI_pointer -> "void*"

(** 将 FFI 类型转换为 C 类型字符串 *)
let c_type_of_ffi_type = function
  | FFI_int -> "int"
  | FFI_float -> "float"
  | FFI_double -> "double"
  | FFI_string -> "char*"
  | FFI_bool -> "int"
  | FFI_void -> "void"
  | FFI_pointer -> "void*"

(** 生成 FFI 声明的 Scheme 代码 *)
let compile_ffi_decl (decl : ffi_decl) : string =
  let args_str = String.concat " " (List.map string_of_ffi_type decl.arg_types) in
  match decl.library with
  | Some lib ->
      Printf.sprintf "(define %s (foreign-procedure \"%s\" (%s) %s))"
        decl.name
        decl.c_name
        args_str
        (string_of_ffi_type decl.return_type)
  | None ->
      (* 内置函数，不需要库 *)
      Printf.sprintf "(define %s (foreign-procedure \"%s\" (%s) %s))"
        decl.name
        decl.c_name
        args_str
        (string_of_ffi_type decl.return_type)

(** 生成 FFI 调用的 Scheme 代码 *)
let compile_ffi_call (func_name : string) (args : expr list) (compile_expr : expr -> string) : string =
  let args_str = String.concat " " (List.map compile_expr args) in
  Printf.sprintf "(%s %s)" func_name args_str

(** 预定义的常用 C 库 FFI 绑定 *)
module Stdlib = struct
  (** printf 函数 *)
  let printf = {
    name = "c_printf";
    c_name = "printf";
    return_type = FFI_int;
    arg_types = [FFI_string];
    library = Some "libc.so.6";
  }

  (** malloc 函数 *)
  let malloc = {
    name = "c_malloc";
    c_name = "malloc";
    return_type = FFI_pointer;
    arg_types = [FFI_int];
    library = Some "libc.so.6";
  }

  (** free 函数 *)
  let free = {
    name = "c_free";
    c_name = "free";
    return_type = FFI_void;
    arg_types = [FFI_pointer];
    library = Some "libc.so.6";
  }

  (** strlen 函数 *)
  let strlen = {
    name = "c_strlen";
    c_name = "strlen";
    return_type = FFI_int;
    arg_types = [FFI_string];
    library = Some "libc.so.6";
  }

  (** sqrt 函数 *)
  let sqrt = {
    name = "c_sqrt";
    c_name = "sqrt";
    return_type = FFI_double;
    arg_types = [FFI_double];
    library = Some "libm.so.6";
  }

  (** sin 函数 *)
  let sin = {
    name = "c_sin";
    c_name = "sin";
    return_type = FFI_double;
    arg_types = [FFI_double];
    library = Some "libm.so.6";
  }

  (** cos 函数 *)
  let cos = {
    name = "c_cos";
    c_name = "cos";
    return_type = FFI_double;
    arg_types = [FFI_double];
    library = Some "libm.so.6";
  }

  (** 所有标准库 FFI 绑定 *)
  let all_decls = [printf; malloc; free; strlen; sqrt; sin; cos]
end

(** 生成所有标准库 FFI 声明 *)
let compile_stdlib_ffi () : string =
  let decls = List.map compile_ffi_decl Stdlib.all_decls in
  String.concat "\n" decls

(** 生成包含 FFI 的完整 Scheme 程序 *)
let compile_with_ffi (expr : Ast.expr) (compile_expr : expr -> string) : string =
  let ffi_decls = compile_stdlib_ffi () in
  let main_code = compile_expr expr in
  Printf.sprintf "(import (chezscheme))\n\n;; FFI 声明\n%s\n\n;; 主程序\n(display %s)\n(newline)\n"
    ffi_decls
    main_code
