# MyLang - 基于 OCaml 的语言开发底座

一个**可扩展的编程语言开发框架**，基于 OCaml 实现。你可以通过设计新的语法和数据结构，快速获得一个工业级验证的语言实现（包含类型推断、字节码编译、WASM 后端、LSP 支持等）。

> **核心理念**: 像搭积木一样设计编程语言 —— 只需定义 AST 和语法，底座提供完整的编译器基础设施。

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
- **泛型 ADT** — `type 'a option = None | Some of 'a`，`type ('a, 'b) result = Ok of 'a | Error of 'b`
- **引用类型** — `ref 42`，`!x`，`x := 20`
- **一等函数** — `fun x -> x + 1`
- **递归** — `let rec factorial = fun n -> ...`
- **模式匹配** — `match xs with | [] -> 0 | h::t -> h | [a, b, c] -> a + b + c | (x, y) -> x + y`
- **异常处理** — `try expr with | Pattern -> handler`，`raise expr`
- **静态类型推断** — Hindley-Milner，支持泛型多态
- **切片语法** — `[1, 2, 3, 4][1:3]`，`"hello"[1:4]`
- **索引访问** — `list[0]`，`string[0]`，`array.(0)`
- **字节码编译器 + 虚拟机** — 栈式 VM 编译执行，支持尾调用优化（TCO）
- **寄存器 VM** — 基于寄存器的虚拟机，显式调用栈消除递归开销
- **JIT 即时编译** — x86-64 机器码生成，通过 mmap RWX 内存真实执行
- **分代垃圾回收** — 年轻代（复制算法）+ 老年代（标记-清除），支持 GC 根追踪
- **Traits（类型类）** — 类似 Rust trait / Haskell Typeclass 的接口抽象，运行时方法分派
- **WASM 后端** — 生成 WebAssembly 文本格式 (.wat)
- **模块系统** — `module M = struct ... end`，`open M`，`M.x`
- **标准库** — Map（AVL 树）、Set、Queue、Stack
- **包管理器** — `my-lang.toml`，支持 `init`/`build`/`install`/`test`
- **LSP 语言服务器** — 代码补全、类型提示、错误诊断
- **高阶函数** — `map`、`filter`、`fold` 内置函数
- **所有权检查** — 移动/借用语义静态分析

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
│   ├── vm.ml         # 栈式字节码虚拟机（帧指针优化）
│   ├── reg_vm.ml     # 寄存器虚拟机（显式调用栈）
│   ├── reg_compiler.ml # 寄存器字节码编译器
│   ├── jit.ml        # JIT x86-64 编译器
│   ├── jit_mmap.c    # JIT mmap RWX 内存 C stub
│   ├── generational_gc.ml # 分代垃圾回收器
│   ├── gc_bridge.ml  # GC 与 VM 桥接层
│   ├── traits.ml     # Traits（类型类）系统
│   ├── ownership.ml  # 所有权/借用检查器
│   ├── wasm_backend.ml # WASM 文本生成
│   ├── package_manager.ml # 包管理器
│   ├── lsp_server.ml # LSP 语言服务器
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

## CLI 用法

### REPL
```bash
$ dune exec my_lang
my-lang> 1 + 2
3
my-lang> let x = 10 in x * 2
20
```

### 运行文件
```bash
$ dune exec my_lang -- examples/fibonacci.ml
```

### 编译
```bash
# 编译为字节码
$ dune exec my_lang -- compile file.ml

# 编译为 WASM
$ dune exec my_lang -- compile --wasm file.ml
```

### 包管理
```bash
# 初始化新项目
$ dune exec my_lang -- init my-project

# 构建项目
$ cd my-project && dune exec my_lang -- build

# 安装依赖
$ dune exec my_lang -- install

# 运行测试
$ dune exec my_lang -- test

# 显示项目信息
$ dune exec my_lang -- info
```

### LSP 服务器
```bash
# 启动 LSP 服务器（用于编辑器集成）
$ dune exec my_lang -- lsp
```

## 示例

### 基础
```ocaml
let factorial = fun n ->
  if n = 0 then 1 else n * factorial (n - 1)
in factorial 5   (* => 120 *)
```

### 模块系统
```ocaml
module Math = struct
  let add = fun x -> fun y -> x + y
  let pi = 314
end;

Math.add 1 2  (* => 3 *)
```

### 泛型 ADT
```ocaml
type 'a option = None | Some of 'a;
let x = Some 42
```

### 高阶函数
```ocaml
let sum = fold (fun acc -> fun x -> acc + x) 0 [1, 2, 3, 4, 5]
(* => 15 *)
```

### Traits（类型类）
```ocaml
trait Show {
  show : string
}

impl Show for int {
  show = fun x -> string_of_int x
}

show 42   (* => "42" *)

(* 自定义 trait *)
trait Doubler {
  double : int
}

impl Doubler for int {
  double = fun x -> x + x
}

double 5   (* => 10 *)
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

## 语言开发框架

本项目不仅是一个具体语言（MyLang），更是一个**语言开发底座**。你可以基于它快速创建自己的编程语言。

### 框架架构

```
framework/
├── common/              # 公共基础设施
│   ├── pos.ml          # 源码位置、错误类型
│   ├── language_intf.ml # 语言接口定义
│   └── pipeline.ml     # 统一执行管线
├── ast/                # 通用 AST 类型
│   └── ast_types.ml    # 所有语言共享的 AST 节点
├── metaprogramming/    # 元编程扩展
│   ├── quote.ml        # Quote/Anti-quote
│   ├── macro.ml        # 宏展开器
│   └── ctfe.ml         # 编译时求值
└── tools/              # 通用工具
    ├── repl.ml         # 交互式解释器
    └── lsp.ml          # LSP 语言服务器
```

### 元编程能力

底座内置了类似 Lisp 的元编程能力：

```ocaml
(* 1. Quote - 代码变数据 *)
quote (1 + 2)  (* => 表示 AST 的数据结构 *)

(* 2. 宏定义 *)
macro unless cond body =
  quote (if ~cond then () else ~body)

(* 3. 编译时求值（常量折叠） *)
let x = 2 * 3 + 4  (* 编译时自动折叠为 10 *)
```

### 如何创建新语言

只需实现 `Language_intf` 接口，即可获得完整的编译器工具链：

```ocaml
module MyLanguage = struct
  module Frontend = struct
    type ast = ...
    let parse source = ...
  end
  
  module TypeSystem = struct
    type ast = Frontend.ast
    type typ = ...
    let typecheck ast = ...
  end
  
  module Evaluator = struct
    type ast = Frontend.ast
    type value = ...
    let eval ast = ...
  end
  
  module Compiler = struct
    type ast = Frontend.ast
    type bytecode = ...
    let compile ast = ...
    let execute bytecode = ...
  end
  
  let name = "MyLang"
  let version = "0.1.0"
end

(* 自动生成 REPL、LSP、包管理器 *)
module MyLangTools = Framework.Tools.Repl.Make (MyLanguage)
module MyLangLsp = Framework.Tools.Lsp.Make (MyLanguage)
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
- [x] 泛型 ADT `type 'a option = None | Some of 'a`
- [x] 引用类型 `ref 42`
- [x] 模式匹配（含列表、元组、cons、构造函数模式）
- [x] 递归函数
- [x] while 循环
- [x] 索引访问
- [x] 切片语法
- [x] 异常处理 `try/raise`
- [x] 类型标注 `let x : int = 42`
- [x] 语法糖（assert、ignore、管道、todo）

### 第二阶段：编译器后端（已完成）
- [x] 字节码编译器 + VM
- [x] 尾调用优化（TCO）
- [x] 异常处理字节码编译
- [x] 切片字节码编译
- [x] 元组/列表模式匹配字节码编译
- [x] WASM 后端（基础实现）
- [x] 垃圾回收器（mark-sweep）

### 第三阶段：工程化（已完成）
- [x] 模块导入
- [x] 模块系统（module/open）
- [x] 高阶函数（map/filter/fold）
- [x] 负整数与二元减法解析修复
- [x] 解释器与字节码一致性验证
- [x] 标准库（Map、Set、Queue、Stack）

### 第四阶段：工业级运行时（已完成）
- [x] **寄存器 VM** — 显式调用栈，消除 OCaml 递归开销
- [x] **JIT 即时编译** — x86-64 机器码 + Linux mmap RWX 真实执行
- [x] **分代 GC** — 年轻代复制 + 老年代标记清除，集成 eval/VM
- [x] **Traits（类型类）** — trait 定义 + impl 实现 + 运行时方法分派
- [x] **所有权检查** — 移动/借用语义静态分析
- [x] **帧指针优化** — 栈 VM 使用 saved_sp 替代 Array.sub 栈副本

### 第五阶段：工具链（已完成）
- [x] 包管理器（my-lang.toml）
- [x] LSP 语言服务器
- [ ] 增量编译

## 许可证

MIT
