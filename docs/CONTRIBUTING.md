# 贡献指南

感谢你对 MyLang 感兴趣！本指南将帮助你理解项目结构，并教你如何扩展语言。

## 项目结构

```
lib/
  ast.ml         - 抽象语法树（AST）定义
  lexer.mll      - 词法分析器（ocamllex）
  parser.mly     - 语法分析器（menhir）
  eval.ml        - 树遍历解释器
  types.ml       - 类型系统
  typeinfer.ml   - Hindley-Milner 类型推断
  bytecode.ml    - 字节码指令定义
  compiler.ml    - AST -> 字节码编译器
  vm.ml          - 字节码虚拟机
```

## 添加新语法特性的标准流程

以添加 **`>`（大于）**运算符为例：

### 1. AST —— 定义新的节点类型

```ocaml
(* lib/ast.ml *)
type expr =
  | ...
  | EGt of expr * expr    (* 新增：e1 > e2 *)
```

### 2. Lexer —— 添加新的词法单元

```ocaml
(* lib/lexer.mll *)
rule read = parse
  | ...
  | ">"  { GT }
```

### 3. Parser —— 添加语法规则

```ocaml
(* lib/parser.mly *)
%token GT

expr:
  | ...
  | e1 = expr GT e2 = expr  { EGt (e1, e2) }
```

### 4. Type Checker —— 定义类型规则

```ocaml
(* lib/typeinfer.ml *)
| EGt (e1, e2) ->
    let t1 = infer env e1 in
    let t2 = infer env e2 in
    unify_ref t1 TInt;
    unify_ref t2 TInt;
    TBool
```

### 5. Evaluator —— 实现求值逻辑

```ocaml
(* lib/eval.ml *)
| EGt (e1, e2) ->
    let v1, _ = eval env e1 in
    let v2, _ = eval env e2 in
    (match v1, v2 with
     | VInt a, VInt b -> (VBool (a > b), env)
     | v1, v2 -> raise (RuntimeError ("类型错误: > 的操作数是 "
         ^ type_of_value v1 ^ " 和 " ^ type_of_value v2
         ^ "，需要整数")))
```

### 6. Compiler —— 编译为字节码（可选）

如果不需要字节码支持，可以跳过。否则：

```ocaml
(* lib/compiler.ml *)
| EGt (e1, e2) -> compile_binop ctx e1 e2 Gt
```

### 7. VM —— 执行字节码（可选）

```ocaml
(* lib/vm.ml *)
| Gt ->
    (match pop (), pop () with
     | VInt b, VInt a -> push (VBool (a > b))
     | v1, v2 -> raise (VMError ("类型错误: > 的操作数是 "
         ^ type_of_vm_value v1 ^ " 和 " ^ type_of_vm_value v2)))
```

### 8. 测试 —— 添加测试用例

```ocaml
(* test/test_my_lang.ml *)
run_test "greater than" "3 > 2" (function VBool true -> true | _ -> false);
```

## 添加控制流特性（如 while 循环）

控制流需要字节码支持，因为涉及跳转指令。

完整的修改涉及 **7 个文件**：

1. **AST** (`lib/ast.ml`)：`EWhile of expr * expr`
2. **Lexer** (`lib/lexer.mll`)：`while`、`do`、`done`
3. **Parser** (`lib/parser.mly`)：`WHILE c = expr DO body = expr DONE`
4. **Type Checker** (`lib/typeinfer.ml`)：条件 `TBool`，返回 `TUnit`
5. **Evaluator** (`lib/eval.ml`)：递归求值条件
6. **Compiler** (`lib/compiler.ml`)：生成 `Jump` / `JumpIfFalse`
7. **VM** (`lib/vm.ml`)：已支持跳转，无需修改

## 添加新数据类型（如记录类型）

记录类型是较大的特性，需要：

1. **AST**：`ERecord of (string * expr) list`，`EField of expr * string`
2. **Lexer**：`{`、`}`、`.`
3. **Parser**：`{ x = 1, y = 2 }`，`r.x`
4. **Type Checker**：记录类型、字段访问类型
5. **Evaluator**：记录值、字段访问求值
6. **Compiler + VM**：记录相关的字节码指令

## 代码规范

- **函数命名**：`snake_case`
- **类型命名**：`PascalCase`
- **注释**：使用 `(* ... *)`，关键函数添加文档注释
- **错误消息**：统一为中文，格式：`"类型错误: ..."`、`"未绑定变量: ..."`

## 提交规范

```
<type>: <描述>

- 详细变更1
- 详细变更2
```

类型：
- **feat**: 新功能
- **fix**: Bug 修复
- **perf**: 性能优化
- **refactor**: 重构
- **docs**: 文档
- **test**: 测试

## 提交前检查清单

- [ ] `dune build` 无错误
- [ ] `dune test` 全部通过
- [ ] 新功能有对应的测试
- [ ] 文档已更新（如需要）

## 获取帮助

- 查看 [ARCHITECTURE.md](ARCHITECTURE.md) 了解架构设计
- 查看 [BEST_PRACTICES.md](BEST_PRACTICES.md) 了解调试技巧
- 在 GitHub Issues 提问
