# MyLang 开发最佳实践

本文档总结了 MyLang 开发过程中的关键经验教训和最佳实践。

---

## 1. VM 设计：避免代码重复

**问题**：VM 需要支持函数调用时，最初的实现将主代码执行和函数体执行分成两个几乎完全相同的函数。

**解决方案**：提取统一的 `exec_instr` 函数处理单条指令，用 `execute_block` 执行代码块。

```ocaml
let rec exec_instr instr =
  match instr with
  | PushInt n -> push (VInt n)
  | LoadVar x -> push (lookup !env x)
  | Call -> (* 递归调用 execute_block *)
  | Return -> (* 恢复调用者状态 *)
  | ...

and execute_block block_code =
  while !pc < Array.length block_code do
    exec_instr block_code.(!pc);
    incr pc
  done
```

---

## 2. AST 设计原则

```ocaml
(* 使用代数数据类型表示 AST *)
type expr =
  | EInt of int
  | EBool of bool
  | EVar of string
  | ELet of string * expr * expr
  | EFun of string list * expr
  | EApp of expr * expr list
  | EIf of expr * expr * expr
  | EBinary of binary_op * expr * expr

(* 区分表达式和值 *)
type value =
  | VInt of int
  | VBool of bool
  | VFun of string list * expr * env
  | VBuiltin of string * (value list -> value)

and env = (string * value) list
```

---

## 3. 类型推断（Hindley-Milner）

```ocaml
let rec infer env = function
  | EInt _ -> TInt
  | EVar x -> lookup env x
  | ELet (x, e1, e2) ->
      let t1 = infer env e1 in
      infer ((x, t1) :: env) e2
  | EFun (params, body) ->
      let param_types = List.map fresh_var params in
      let body_type = infer (combine params param_types @ env) body in
      fold_right TArrow param_types body_type
```

---

## 4. 错误处理：Result Monad

```ocaml
let (let*) = Result.bind

let eval env = function
  | EDiv (e1, e2) ->
      let* v1 = eval env e1 in
      let* v2 = eval env e2 in
      match v1, v2 with
      | VInt _, VInt 0 -> Error "Division by zero"
      | VInt a, VInt b -> Ok (VInt (a / b))
      | _ -> Error "Type error"
```

---

## 5. 性能优化

### 常量折叠
```ocaml
let fold_constants = function
  | EBinary (Add, EInt a, EInt b) -> EInt (a + b)
  | EIf (EBool true, t, _) -> t
  | EIf (EBool false, _, e) -> e
  | e -> e
```

### 快速路径
```ocaml
let eval env = function
  (* 快速路径：常量 *)
  | EInt n -> Ok (VInt n, env)
  | EBool b -> Ok (VBool b, env)
  (* 快速路径：变量查找 *)
  | EVar x -> Ok (lookup_fast env x, env)
  (* 快速路径：整数算术 *)
  | EAdd (EInt a, EInt b) -> Ok (VInt (a + b), env)
  (* 通用路径 *)
  | EAdd (e1, e2) -> eval_binop env e1 e2 (+)
```

---

## 6. 测试策略

```ocaml
(* 使用 alcotest *)
let test_int_arithmetic () =
  let result = My_lang.run "1 + 2" in
  check string "1 + 2 = 3" "3" (string_of_value result)

let () =
  run "MyLang" [
    "arithmetic", [
      test_case "integer arithmetic" `Quick test_int_arithmetic;
    ];
  ]
```

---

## 7. 模块化架构

```
lib/
├── ast.ml           # AST 定义
├── lexer.mll        # 词法分析器
├── parser.mly       # 语法分析器
├── eval_helpers.ml  # 求值器辅助函数
├── eval_pattern.ml  # 模式匹配
├── eval_builtin.ml  # 内置函数
├── eval.ml          # 核心求值器
├── compiler.ml      # 字节码编译器
├── vm.ml            # 虚拟机
└── my_lang.ml       # 库入口
```

---

## 8. 常见陷阱

1. **环境传递**：确保环境在函数调用时正确传递
2. **闭包捕获**：闭包应捕获定义时的环境
3. **模式匹配穷尽**：确保所有模式都被处理
4. **尾调用优化**：识别尾递归并优化
5. **内存泄漏**：注意循环引用

---

## 9. 调试技巧

```ocaml
(* 添加调试输出 *)
let debug_eval env expr =
  Printf.printf "eval: %s\n" (string_of_expr expr);
  let result = eval env expr in
  Printf.printf "result: %s\n" (string_of_value result);
  result
```
