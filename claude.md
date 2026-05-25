# MyLang 开发最佳实践

> 本文档总结了 MyLang 开发过程中的关键经验教训和最佳实践，
> 涵盖 VM 设计、编译器开发、调试技巧和代码质量维护。

---

## 1. VM 设计：避免代码重复

### 问题
当 VM 需要支持函数调用时，最初的实现将主代码执行和函数体执行分成两个几乎完全相同的函数（`execute` 和 `execute_func`），导致约 **250 行重复代码**。

### 解决方案
提取统一的 `exec_instr` 函数处理单条指令，用 `execute_block` 执行代码块：

```ocaml
let rec exec_instr instr =
  match instr with
  | PushInt n -> push (VInt n)
  | LoadVar x -> push (lookup !env x)
  | Call -> (* 递归调用 execute_block *)
  | Return -> (* 恢复调用者状态或抛出 ReturnExn *)
  | ...

and execute_block block_code =
  while !pc < Array.length block_code do
    let instr = block_code.(!pc) in
    pc := !pc + 1;
    exec_instr instr
  done
```

### 原则
- **单一职责**：指令执行逻辑只写一次
- **递归调用**：`Call` 指令通过递归调用 `execute_block` 进入函数体
- **异常控制流**：使用局部异常（`let exception ReturnExn`）安全跳出嵌套循环

---

## 2. 调试技巧：VM 执行跟踪

### 方法
在 `exec_instr` 入口处添加调试输出，打印当前状态：

```ocaml
let rec exec_instr instr =
  Printf.printf "DEBUG pc=%d instr=%s env=[" 
    !pc (Bytecode.string_of_instr instr);
  List.iter (fun (k,_) -> Printf.printf "%s;" k) !env;
  Printf.printf "]\n";
  match instr with ...
```

### 关键观察点
- **环境变化**：`StoreVar` 后检查变量是否正确绑定
- **调用栈**：`Call` 时确认参数和闭包环境
- **返回点**：`Return` 时验证调用栈恢复是否正确

### 原则
- 调试输出应可快速启用/禁用
- 关注环境（env）和栈（stack）的对应关系
- 递归函数调用是 bug 高发区，需逐层验证

---

## 3. 常见 Bug 模式：Return 指令处理

### 问题
当递归函数返回时，如果最外层 `execute_block` 的 `pc` 未被正确终止，
VM 会继续执行主代码中的后续指令，而此时环境已被恢复，导致 **Unbound variable** 错误。

### 错误场景
```
factorial 5 -> 返回 120 到主函数
主函数继续执行 pc=6 (Jump 13) 
但此时 env=[factorial]（没有 n）
pc=7 (LoadVar n) -> 报错：Unbound variable: n
```

### 解决方案
使用局部异常跳出当前代码块：

```ocaml
let exception ReturnExn in

let rec exec_instr instr =
  match instr with
  | Return ->
      (match !call_stack with
       | (old_pc, old_stack, old_env) :: rest ->
           (* 恢复调用者状态 *)
           push result
       | [] ->
           (* 最外层返回：抛出异常 *)
           raise ReturnExn)
  | ...

and execute_block block_code =
  try
    while !pc < Array.length block_code do
      ...
    done
  with ReturnExn -> ()
```

### 原则
- **区分返回层级**：函数内部返回 vs 最外层返回
- **避免手动修改 pc**：不要通过 `pc := Array.length code` 来退出循环
- **使用异常**：局部异常是跳出嵌套循环的安全方式

---

## 4. 编译器开发：明确错误处理

### 问题
编译器对不支持的表达式或模式使用 `_ -> emit ctx PushUnit`，导致：
- 静默忽略错误
- 运行时行为不可预期
- 调试困难

### 解决方案
对未实现的功能使用显式 `failwith`：

```ocaml
let rec compile_expr ctx expr =
  match expr with
  | EInt n -> emit ctx (PushInt n)
  | EMatch (e, cases) -> compile_match ctx e cases
  | _ -> failwith "compile_expr: unsupported expression"

and compile_match ctx e cases =
  match cases with
  | (PInt n, body) :: rest -> ...
  | (PBool _, _) :: _ ->
      failwith "compile_match: boolean patterns not yet supported in bytecode"
  | (PCons _, _) :: _ ->
      failwith "compile_match: cons patterns not yet supported in bytecode"
```

### 原则
- **fail fast**：在编译期捕获错误，而非运行时
- **清晰错误信息**：包含位置（函数名）和原因
- **区分层级**：解释器支持 ≠ 字节码支持，需分别标注

---

## 5. 注释规范

### 模块级文档
每个 `.ml` 文件开头应有模块级文档注释：

```ocaml
(** 模块功能概述

    简要说明模块的职责和主要数据结构。
    
    核心概念：
    - 概念1：解释...
    - 概念2：解释...
*)
```

### 函数文档
关键函数应包含：
- **功能说明**：函数做什么
- **参数说明**：每个参数的语义
- **返回值**：返回什么
- **副作用**：是否修改全局状态
- **异常**：可能抛出的异常

```ocaml
(** 应用替换到类型

    [apply subst t] 将类型 [t] 中所有出现在替换域中的类型变量
    替换为对应类型。
    
    时间复杂度：O(|t| * log|subst|)
*)
let rec apply subst t = ...
```

### 内联注释
- 复杂算法步骤需逐行注释
- 控制流（if/match/loop）注释分支意图
- 调试代码标记 `(* DEBUG: ... *)`

---

## 6. 测试策略

### 测试分层
```
解释器测试（test_my_lang.ml）
├── 基础类型：int, bool, string
├── 复合类型：list, tuple
├── 控制流：if, match
├── 函数：lambda, let rec
└── 错误处理：类型错误, 运行时错误

字节码测试（test_bytecode.ml）
├── 编译正确性
├── 执行正确性
└── 与解释器结果一致性
```

### 关键测试用例
- **递归函数**：验证调用栈和环境的正确保存/恢复
- **模式匹配**：验证各分支类型一致性
- **错误场景**：除零、未绑定变量、类型不匹配

### 测试驱动修复
1. 发现 bug 时先编写最小复现测试
2. 运行测试确认失败
3. 修复代码
4. 运行全部测试确认无回归

---

## 7. 性能优化原则

### 已验证的优化
| 优化点 | 原实现 | 优化后 | 影响 |
|--------|--------|--------|------|
| 类型替换 | (int * t) list | Int.Map | O(n) → O(log n) |
| 指令累积 | Array.append | list 累积 + 反转 | O(n²) → O(n) |
| VM 状态 | 纯函数 | mutable refs | 减少 GC 压力 |

### 优化原则
- **先测量，后优化**：确保优化有实际效果
- **保持正确性**：优化后必须跑通全部测试
- **局部化变更**：一次只改一个模块，便于回滚

---

## 8. Git 工作流

### 提交规范
```
类型：简短描述

- 详细变更1
- 详细变更2
- 详细变更3
```

类型包括：
- **feat**: 新功能
- **fix**: Bug 修复
- **perf**: 性能优化
- **refactor**: 重构
- **docs**: 文档
- **test**: 测试

### 提交前检查清单
- [ ] `dune build` 无错误无警告
- [ ] `dune test` 全部通过
- [ ] 变更文件已审查（`git diff --stat`）
- [ ] 无调试代码残留

---

## 9. 常见陷阱

### OCaml 特有
- **`let exception` 作用域**：局部异常必须在 `let ... in` 中定义
- **`ref` 的不可变性**：`ref` 本身不可变，其内容可变，注意不要重新绑定 ref
- **列表反转**：`emit` 使用 `instr :: code` 累积，最终需 `List.rev`

### 编译器/VM 交互
- **AST 修改影响**：修改 `Ast.expr` 需同步更新 parser、compiler、eval、typeinfer
- **字节码一致性**：新增指令需同步更新 bytecode.ml、compiler.ml、vm.ml
- **递归闭包**：`let rec` 闭包需通过 `let rec self = VClosure (...)` 实现自引用

---

*最后更新：2026-05-25*
