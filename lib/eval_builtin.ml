(** 内置函数环境 - 兼容性包装器

    所有实现已迁移至 Builtins 模块。此模块保留以维持向后兼容。
*)

type eval_context = Builtins.eval_context = {
  eval_fn: Ast.env -> Ast.expr -> (Ast.value * Ast.env, string) Result.t;
  apply_fn: Ast.env -> Ast.value -> Ast.value -> (Ast.value * Ast.env, string) Result.t;
}

let create_builtin_env = Builtins.create_builtin_env
