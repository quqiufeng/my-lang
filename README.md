# MyLang - OCaml 编程语言实现样板

一个**简单但完整的函数式编程语言实现**，可作为你自己的编程语言的起点。

## 快速开始

```bash
# 克隆项目
git clone https://github.com/quqiufeng/my-lang.git
cd my-lang

# 构建
eval $(opam env)
dune build

# 运行 REPL
dune exec my_lang

# 运行示例
dune exec my_lang -- examples/fibonacci.ml

# 测试
dune test
```

## 功能特性

- **基本数据类型** — 整数、布尔值、字符串、字符、单元
- **复合数据类型** — 列表 `[1, 2, 3]`、元组 `(1, true)`、数组 `[|1, 2, 3|]`、记录 `{name = "x"; age = 1}`
- **代数数据类型（ADT）** — `type color = Red | Green | Blue`
- **引用类型** — `ref 42`，`!x`，`x := 20`
- **一等函数** — `fun x -> x + 1`
- **递归** — `let rec factorial = fun n -> ...`
- **模式匹配** — `match xs with | [] -> 0 | h::t -> h | [a, b, c] -> a + b + c | (x, y) -> x + y`
- **异常处理** — `try expr with | Pattern -> handler`，`raise expr`
- **静态类型推断** — Hindley-Milner，无需类型标注
- **切片语法** — `[1, 2, 3, 4][1:3]`，`"hello"[1:4]`
- **索引访问** — `list[0]`，`string[0]`，`array.(0)`
- **字节码编译器 + 虚拟机** — 编译执行提升性能，支持尾调用优化
- **模块导入** — `import "stdlib.ml"`
- **高阶函数** — `map`、`filter`、`fold` 内置函数

## 项目结构

```
my-lang/
├── bin/              # CLI / REPL
├── lib/              # 核心库
│   ├── ast.ml        # 抽象语法树
│   ├── lexer.mll     # 词法分析器
│   ├── parser.mly    # 语法分析器
│   ├── eval.ml       # 树遍历解释器
│   ├── typeinfer.ml  # Hindley-Milner 类型推断
│   ├── compiler.ml   # AST -> 字节码编译器
│   ├── vm.ml         # 字节码虚拟机
│   └── my_lang.ml    # 库入口
├── test/             # 测试套件
├── examples/         # 示例程序
│   ├── language/     # 语言特性展示
│   ├── stdlib/       # 标准库
│   └── advanced/     # 高级示例
└── docs/             # 文档
    ├── ARCHITECTURE.md   # 架构设计
    ├── BEST_PRACTICES.md # 开发最佳实践
    └── CONTRIBUTING.md   # 扩展指南
```

## 示例

### 基础
```ocaml
let factorial = fun n ->
  if n = 0 then 1 else n * factorial (n - 1)
in factorial 5   (* => 120 *)
```

### 高阶函数
```ocaml
let sum = fold (fun acc -> fun x -> acc + x) 0 [1, 2, 3, 4, 5]
(* => 15 *)
```

### 模式匹配
```ocaml
match [1, 2, 3] with
| [] -> 0
| h :: t -> h + length t   (* => 3 *)
```

### 导入模块
```ocaml
let _ = import "examples/stdlib/list.ml" in
let doubled = map (fun x -> x * 2) [1, 2, 3]
(* => [2, 4, 6] *)
```

## 扩展你自己的语言

想要基于此项目创建自己的语言？查看：

- **[docs/CONTRIBUTING.md](docs/CONTRIBUTING.md)** — 如何添加新语法、类型、优化
- **[docs/ARCHITECTURE.md](docs/ARCHITECTURE.md)** — 深入了解编译器架构
- **[docs/BEST_PRACTICES.md](docs/BEST_PRACTICES.md)** — 开发最佳实践和调试技巧

## 为什么选择 OCaml？

许多成功的语言最初都用 OCaml 开发：Rust、Coq、F*、MirageOS...

OCaml 的**代数数据类型**和**模式匹配**让编译器实现变得异常简洁，而**类型推断**则确保代码正确性。

## 路线图

### 第一阶段：语言核心（已完成）
- [x] 核心语言（变量、函数、if）
- [x] 类型推断（Hindley-Milner）
- [x] 列表、元组、字符串、字符
- [x] 数组 `[|1, 2, 3|]`
- [x] 记录 `{name = "x"; age = 1}`
- [x] 代数数据类型（ADT）`type color = Red | Green | Blue`
- [x] 引用类型 `ref 42`
- [x] 模式匹配（含列表、元组、cons、构造函数模式）
- [x] 递归函数
- [x] while 循环
- [x] 索引访问
- [x] 切片语法
- [x] 异常处理 `try/raise`

### 第二阶段：编译器后端（已完成）
- [x] 字节码编译器 + VM
- [x] 尾调用优化（TCO）
- [x] 异常处理字节码编译
- [x] 切片字节码编译
- [x] 元组/列表模式匹配字节码编译

### 第三阶段：工程化（进行中）
- [x] 模块导入
- [x] 高阶函数（map/filter/fold）
- [x] 负整数与二元减法解析修复
- [x] 解释器与字节码一致性验证
- [ ] 类型标注（`: int`, `: bool`）
- [ ] 垃圾回收
- [ ] WASM 后端
- [ ] JIT 编译

## 许可证

MIT
