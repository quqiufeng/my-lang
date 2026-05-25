(** 元编程接口定义

    定义宏系统、Quote、编译时求值等元编程功能。
    新语言可以通过实现这些接口来获得元编程能力。
*)

(** AST 转换器：用于宏展开 *)
module type AstTransformer = sig
  type ast
  
  (** 遍历并转换 AST *)
  val map : (ast -> ast) -> ast -> ast
  
  (** 折叠 AST *)
  val fold : ('a -> ast -> 'a) -> 'a -> ast -> 'a
  
  (** 检查 AST 中是否包含特定模式 *)
  val exists : (ast -> bool) -> ast -> bool
end

(** Quote/Anti-quote 接口 *)
module type Quoting = sig
  type ast
  type quoted_ast  (* AST 的 AST 表示，即 quote 后的值 *)
  
  (** 将 AST 包装为 quote 节点 *)
  val quote : ast -> ast
  
  (** 在 quote 中插入外部值（anti-quote） *)
  val anti_quote : ast -> ast
  
  (** 将 AST 转为可操作的 quoted_ast *)
  val lift : ast -> quoted_ast
  
  (** 将 quoted_ast 降回 AST *)
  val unlift : quoted_ast -> ast
  
  (** 评估 quoted_ast（如果它包含可计算的部分） *)
  val eval_quoted : quoted_ast -> quoted_ast
end

(** 宏接口 *)
module type Macros = sig
  type ast
  
  (** 宏定义：名称 × 参数列表 × 展开函数 *)
  type macro_def = {
    name : string;
    params : string list;
    expand : ast list -> ast;  (* 参数列表 -> 展开后的 AST *)
  }
  
  (** 宏环境 *)
  type macro_env
  
  (** 创建空的宏环境 *)
  val empty_env : unit -> macro_env
  
  (** 注册宏 *)
  val define_macro : macro_env -> macro_def -> unit
  
  (** 展开 AST 中的所有宏 *)
  val expand_macros : macro_env -> ast -> ast
  
  (** 检查 AST 中是否还有未展开的宏 *)
  val has_macros : ast -> bool
end

(** 编译时求值接口 *)
module type Ctfe = sig
  type ast
  type value
  
  (** 在编译时求值表达式 *)
  val eval_at_compile_time : ast -> value option
  
  (** 将值嵌入 AST（常量折叠） *)
  val embed_value : value -> ast
  
  (** 常量折叠：编译时求值所有可求值的子表达式 *)
  val constant_fold : ast -> ast
end

(** 完整的元编程扩展 *)
module type Metaprogramming = sig
  type ast
  type value
  
  module Transformer : AstTransformer with type ast = ast
  module Quoting : Quoting with type ast = ast
  module Macros : Macros with type ast = ast
  module Ctfe : Ctfe with type ast = ast and type value = value
end
