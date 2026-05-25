# MyLang

一门用 OCaml 实现的简单函数式编程语言。

## 为什么选择 OCaml？

OCaml 是开发编程语言的**秘密武器**。许多有影响力的语言最初都是用 OCaml 开发原型，随后才被重写为目标语言：

| 语言 | 初始原型 | 迁移原因 |
|------|---------|---------|
| **Rust** | OCaml | 证明类型系统后自举 |
| **Coq** | OCaml（至今仍是）| 依赖类型证明助手 |
| **F\*** | OCaml（至今仍是）| 面向验证的 ML 方言 |
| **MirageOS** | OCaml（至今仍是）| 单内核操作系统 |
| **ReasonML/Rescript** | OCaml（至今仍是）| 面向 JS 的语法层 |
| **Elm**（早期版本）| OCaml | 函数式 Web 前端 |

### 为什么 OCaml 特别适合开发语言？

**1. 代数数据类型 = 完美的 AST 表示**

编程语言的本质是树。OCaml 的 `type` 声明让你可以直接、精确地建模 AST：

```ocaml
type expr =
  | EInt of int
  | EAdd of expr * expr
  | ELet of string * expr * expr
  | EFun of string * expr
```

没有空指针，没有继承困扰，没有访问者模式样板代码。类型本身就是语法。

**2. 模式匹配 = 树遍历变得轻而易举**

编译器 80% 的时间都在遍历树。OCaml 的 `match` 让这变得既安全又优雅：

```ocaml
let rec eval = function
  | EInt n -> VInt n
  | EAdd (e1, e2) -> VInt (eval_int e1 + eval_int e2)
  | ELet (x, e1, e2) -> eval (bind x (eval e1) env) e2
```

如果你漏掉了一个分支，编译器会警告你。不需要 `instanceof` 链，不需要 `if/else` 梯子。

**3. 强静态类型在编译期捕获 Bug**

当你在处理 AST 节点、环境和字节码指令时，类型安全不是奢侈品，而是生存必需品。OCaml 的类型推断能在运行前就捕获"你在需要值的地方传了表达式"这类错误。

**4. 垃圾回收 = 专注语义，而非内存**

语言开发已经够难了，没必要再手动管理内存。OCaml 的 GC 让你可以构建复杂的数据结构（环境、闭包、替换映射）而无需考虑生命周期。

**5. 开箱即用的成熟工具链**

- `ocamllex`：从正则规则生成词法分析器
- `menhir`：从 BNF 文法生成 LR(1) 语法分析器
- `dune`：现代构建系统，支持增量编译
- `merlin`/`ocaml-lsp`：IDE 支持，提供类型提示和跳转到定义

你不需要手写分词器或解析器。定义好文法就可以开始了。

**6. 函数式范式 = 编译器的天然契合**

编译器是纯函数：`AST -> AST -> Bytecode`。不可变性让转换易于推理。高阶函数让你可以抽象常见模式（对 AST 做 map、对表达式做 fold）。

### 取舍

OCaml 并非完美适用于语言开发的每个阶段：

- **擅长**：前端（解析、类型检查、AST 转换）、快速原型、正确性关键的代码
- **不太适合**：底层 VM 后端（JIT/AOT 最终需要 Rust/C++）、超低延迟 GC、大规模并行

这正是 **Rust 最初用 OCaml 开发原型** 的原因——先证明类型系统和借用检查器的逻辑，然后再将性能关键部分用系统语言重写。

---

## 如何实现一门编程语言（用 OCaml）

实现一门语言比你想象的简单。你只需将其分解为**四个阶段**：

```
源代码 → 词法分析 → 语法分析 → 抽象语法树 → 求值器 → 结果
  "1+2"     词法单元   语法树    值
```

### 1. 词法分析器（Lexer）

将原始文本转换为**词法单元（token）**流，即最小的有意义的单元。

```ocaml
(* lib/lexer.mll *)
rule read = parse
  | digit+ as n   { INT (int_of_string n) }   (* "42" → INT 42 *)
  | "+"           { PLUS }
  | "let"         { LET }
  | ident as s    { IDENT s }                 (* "x" → IDENT "x" *)
  | whitespace    { read lexbuf }             (* 跳过空格 *)
  | eof           { EOF }
```

输入：`"let x = 1 + 2"`  
输出：`[LET; IDENT "x"; EQ; INT 1; PLUS; INT 2; EOF]`

### 2. 语法分析器（Parser）

将词法单元转换为**抽象语法树（AST）**——一棵树，表示程序的*结构*，忽略括号、关键字等语法噪音。

```ocaml
(* lib/parser.mly *)
expr:
  | n = INT                      { EInt n }
  | e1 = expr PLUS e2 = expr     { EAdd (e1, e2) }
  | LET x = IDENT EQ e1 = expr IN e2 = expr
                                 { ELet (x, e1, e2) }
  | x = IDENT                    { EVar x }
  ;
```

输入：`[LET; IDENT "x"; EQ; INT 1; PLUS; INT 2; EOF]`  
输出：`ELet ("x", EAdd (EInt 1, EInt 2), EVar "x")`

### 3. 抽象语法树（AST）

你的语言的心脏。你用 OCaml 的代数数据类型定义程序*是什么*。

```ocaml
(* lib/ast.ml *)
type expr =
  | EInt of int              (* 42 *)
  | EBool of bool            (* true *)
  | EVar of string           (* x *)
  | EAdd of expr * expr      (* e1 + e2 *)
  | ELet of string * expr * expr   (* let x = e1 in e2 *)
  | EFun of string * expr    (* fun x -> e *)
  | EApp of expr * expr      (* f arg *)
```

### 4. 求值器（Evaluator）

递归遍历 AST 并计算结果。**环境**（变量绑定列表）跟踪每个名字的含义。

```ocaml
(* lib/eval.ml *)
let rec eval env expr =
  match expr with
  | EInt n -> VInt n
  | EVar x -> lookup env x
  | EAdd (e1, e2) ->
      (match eval env e1, eval env e2 with
       | VInt a, VInt b -> VInt (a + b))
  | ELet (x, e1, e2) ->
      let v = eval env e1 in
      eval ((x, v) :: env) e2
  | EFun (param, body) -> VFun (param, body, env)
  | EApp (func, arg) ->
      let VFun (p, body, closure_env) = eval env func in
      let v = eval env arg in
      eval ((p, v) :: closure_env) body
```

核心思想：**函数捕获其定义时的环境**（这就是闭包）。当你调用 `f 5` 时，`f` 运行的是它*被定义时*能看到的变量，而不是它*被调用时*的变量。

---

## 功能特性

- **整数运算**：`+`、`-`、`*`、`/`
- **布尔逻辑**：`&&`、`||`、`not`
- **比较运算**：`=`、`<>`、`<`、`<=`、`>`、`>=`
- **变量绑定**：`let x = expr in expr`
- **递归绑定**：`let rec f = fun x -> ... in ...`
- **一等函数**：`fun x -> expr`
- **条件表达式**：`if expr then expr else expr`
- **字符串**：`"hello world"`，拼接 `^`
- **列表**：`[1, 2, 3]`，`1 :: [2, 3]`
- **元组**：`(1, true, "hello")`
- **顺序执行**：`expr1; expr2`
- **模式匹配**：`match expr with | pattern -> expr | ...`
- **while 循环**：`while cond do body done`
- **索引访问**：`list[0]`，`string[0]`
- **静态类型推断**：Hindley-Milner 类型推断 + let-多态性
- **字节码编译器 + 虚拟机**：编译为字节码以获得更高性能
- **尾调用优化**：递归调用复用栈帧
- **模块导入**：`import "file.ml"`

---

## 如何添加新的语法特性

让我们以添加 **`>`（大于）**运算符为例。你需要修改 **4 个文件**：

### 第 1 步：AST —— 添加新的表达式节点

```ocaml
(* lib/ast.ml *)
type expr =
  | ...
  | EGt of expr * expr    (* 新增：e1 > e2 *)
```

### 第 2 步：词法分析器 —— 添加新的词法单元

```ocaml
(* lib/lexer.mll *)
rule read = parse
  | ...
  | ">"           { GT }    (* 新增 *)
```

### 第 3 步：语法分析器 —— 添加文法规则

```ocaml
(* lib/parser.mly *)
%token GT                    (* 新增 *)
%nonassoc EQ NEQ LT LE GT GE

expr:
  | ...
  | e1 = expr GT e2 = expr  { EGt (e1, e2) }   (* 新增 *)
```

### 第 4 步：求值器 —— 定义它的行为

```ocaml
(* lib/eval.ml *)
let rec eval env expr =
  match expr with
  | ...
  | EGt (e1, e2) ->
      (match eval env e1, eval env e2 with
       | VInt a, VInt b -> VBool (a > b))
```

就这样。用 `dune build` 重新构建，`3 > 2` 现在会求值为 `true`。

### 添加循环特性（例如 `while`）

`while` 需要**字节码支持**，因为循环涉及跳转指令。完整的修改：

1. **AST** (`lib/ast.ml`)：添加 `EWhile of expr * expr`
2. **词法分析** (`lib/lexer.mll`)：添加 `while`、`do`、`done` 关键字
3. **语法分析** (`lib/parser.mly`)：添加 `WHILE c = expr DO body = expr DONE` 规则
4. **类型推断** (`lib/typeinfer.ml`)：条件为 `TBool`，返回 `TUnit`
5. **求值器** (`lib/eval.ml`)：递归求值条件，为 `true` 时求值循环体
6. **编译器** (`lib/compiler.ml`)：生成循环跳转（`Jump` / `JumpIfFalse`）
7. **虚拟机** (`lib/vm.ml`)：已支持跳转指令，无需修改

详细的编译器/虚拟机开发实践请参阅 `claude.md`。

### 添加索引访问（例如 `list[0]`）

索引访问涉及新的语法形式 `e1[e2]`，需要修改：

1. **AST** (`lib/ast.ml`)：添加 `EIndex of expr * expr`
2. **语法分析** (`lib/parser.mly`)：添加 `e1 = simple_expr LBRACKET e2 = expr RBRACKET`
3. **类型推断** (`lib/typeinfer.ml`)：`e1` 为 `TList t_elem` 或 `TString`，`e2` 为 `TInt`
4. **求值器** (`lib/eval.ml`)：验证边界，返回元素
5. **字节码** (`lib/bytecode.ml`)：添加 `Index` 指令
6. **编译器** (`lib/compiler.ml`)：编译为 `Index` 指令
7. **虚拟机** (`lib/vm.ml`)：执行索引访问

---

## 示例程序

```ocaml
(* 算术 *)
1 + 2 * 3        (* => 7 *)

(* 变量绑定 *)
let x = 10 in x + 5   (* => 15 *)

(* 函数 *)
let add = fun x -> fun y -> x + y in add 3 4   (* => 7 *)

(* 递归 *)
let rec factorial = fun n ->
  if n = 0 then 1 else n * factorial (n - 1)
in factorial 5   (* => 120 *)

(* 字符串 *)
let greeting = "Hello" in greeting ^ " World"   (* => "Hello World" *)

(* 列表 *)
let xs = [1, 2, 3] in 1 :: xs   (* => [1, 1, 2, 3] *)

(* 元组 *)
let pair = (1, "hello") in pair   (* => (1, "hello") *)

(* 模式匹配 *)
match [1, 2, 3] with
| [] -> 0
| h :: t -> h + length t   (* => 3 *)

(* 顺序执行 *)
let x = 1 in
let y = 2 in
x + y; x * y   (* => 2 *)

(* 类型推断 - 多态性 *)
let id = fun x -> x in
(id 5, id true)   (* => (5, true) *)

(* while 循环 *)
let i = 1 in
while i < 5 do i + 1 done   (* => () *)

(* 索引访问 *)
[10, 20, 30][1]              (* => 20 *)
"hello"[1]                   (* => "e" *)
```

---

## 快速开始

### 构建

```bash
eval $(opam env --switch=default)
dune build
```

### 运行 REPL

```bash
dune exec my_lang
```

### 运行文件

```bash
dune exec my_lang -- examples/test.ml
```

### 测试

```bash
dune test
```

---

## 项目结构

```
lib/
  ast.ml         - 抽象语法树定义
  lexer.mll      - 词法分析器（ocamllex）
  parser.mly     - 语法分析器（menhir）
  eval.ml        - 树遍历解释器
  types.ml       - 类型系统与统一化
  typeinfer.ml   - Hindley-Milner 类型推断
  bytecode.ml    - 字节码指令定义
  compiler.ml    - AST 到字节码的编译器
  vm.ml          - 基于栈的虚拟机
  my_lang.ml     - 库入口点
bin/
  main.ml        - 命令行 / REPL
test/
  test_my_lang.ml   - 解释器测试
  test_bytecode.ml  - 字节码虚拟机测试
design.md        - 详细设计文档
claude.md        - 开发最佳实践
```

---

## 架构

### 解释器模式（默认）
```
源代码 → 词法分析 → 语法分析 → 抽象语法树 → 类型检查器 → 求值器 → 值
```

### 字节码模式
```
源代码 → 词法分析 → 语法分析 → 抽象语法树 → 编译器 → 字节码 → 虚拟机 → 值
```

**关键设计决策：**
- **词法作用域**：函数捕获其定义时的环境（闭包）
- **严格求值**：函数调用前先求值参数
- **let-多态性**：`let id = fun x -> x` 获得类型 `'a -> 'a`
- **双执行模式**：解释器追求简洁，字节码虚拟机追求性能

---

## 实现说明

- **词法分析器**：使用 `ocamllex` 分词；跟踪行号/列号位置以报告错误
- **语法分析器**：使用 `menhir` 进行 LR(1) 文法解析，处理移进/归约冲突
- **类型检查器**：Hindley-Milner 类型推断，使用 `Int.Map` 实现 O(log n) 替换
- **求值器**：树遍历解释器，环境链式传递
- **编译器**：列表中累积指令（O(1) 追加），使用回填技术处理控制流
- **虚拟机**：基于栈，使用可变引用以减少内存分配；通过 `ReturnExn` 异常处理递归
- **错误处理**：所有错误（语法、解析、类型、运行时、虚拟机）都会被捕获并报告源位置

---

## 路线图

- [x] 核心语言（整数、布尔值、let、函数、if）
- [x] 字符串和拼接
- [x] 列表和 cons 运算符
- [x] 元组
- [x] `let rec` 递归函数
- [x] 顺序执行表达式（`e1; e2`）
- [x] 模式匹配
- [x] Hindley-Milner 类型推断
- [x] 模块导入（`import "file.ml"`）
- [x] 字节码编译器 + 虚拟机
- [x] while 循环
- [x] 列表/字符串索引访问
- [x] 尾调用优化（TCO）
- [ ] 代数数据类型（ADT）
- [ ] 类型标注（`: int`、`: bool`）
- [ ] 垃圾回收

---

## 许可证

MIT
