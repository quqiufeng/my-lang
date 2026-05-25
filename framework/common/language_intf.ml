(** 语言接口定义

    每个具体语言需要实现的接口。
    框架基于这些接口提供通用的工具链（REPL、LSP、包管理器等）。
*)

(** 前端接口：负责源码 -> AST *)
module type Frontend = sig
  type ast
  
  (** 从字符串解析 *)
  val parse : string -> ast
  
  (** 从文件解析 *)
  val parse_file : string -> ast
  
  (** AST 的字符串表示（用于调试） *)
  val dump_ast : ast -> string
end

(** 类型系统接口：负责 AST -> 类型检查 *)
module type TypeSystem = sig
  type ast
  type typ
  type env = (string * typ) list
  
  (** 基础类型环境（内置函数类型） *)
  val builtin_env : env
  
  (** 类型检查 *)
  val typecheck : ast -> typ
  
  (** 在指定环境下类型检查 *)
  val typecheck_with_env : env -> ast -> typ
  
  (** 类型的字符串表示 *)
  val string_of_type : typ -> string
end

(** 求值器接口：负责 AST -> 值 *)
module type Evaluator = sig
  type ast
  type value
  type env = (string * value) list
  
  (** 基础环境（内置函数） *)
  val builtin_env : env
  
  (** 解释执行 AST *)
  val eval : ast -> value
  
  (** 在指定环境下求值 *)
  val eval_with_env : env -> ast -> value
  
  (** 值的字符串表示 *)
  val string_of_value : value -> string
end

(** 编译器接口：负责 AST -> 字节码 -> 执行 *)
module type Compiler = sig
  type ast
  type bytecode
  type vm_value
  
  (** 编译为字节码 *)
  val compile : ast -> bytecode
  
  (** 字节码的字符串表示（反汇编） *)
  val disassemble : bytecode -> string
  
  (** 执行字节码 *)
  val execute : bytecode -> vm_value
  
  (** VM 值的字符串表示 *)
  val string_of_vm_value : vm_value -> string
end

(** 后端接口：负责字节码 -> 目标代码 *)
module type Backend = sig
  type bytecode
  type target_code
  
  (** 生成目标代码 *)
  val generate : bytecode -> target_code
  
  (** 目标代码的字符串表示 *)
  val string_of_target : target_code -> string
end

(** 完整的语言接口 *)
module type Language = sig
  module Frontend : Frontend
  module TypeSystem : TypeSystem with type ast = Frontend.ast
  module Evaluator : Evaluator with type ast = Frontend.ast
  module Compiler : Compiler with type ast = Frontend.ast
  
  type ast = Frontend.ast
  type value = Evaluator.value
  type typ = TypeSystem.typ
  type bytecode = Compiler.bytecode
  type vm_value = Compiler.vm_value
  
  (** 语言元信息 *)
  val name : string
  val version : string
  val description : string
  
  (** 统一的管线执行 *)
  val run : string -> (value, string) result
  val run_file : string -> (value, string) result
  val compile_and_run : string -> (vm_value, string) result
end
