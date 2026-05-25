# MyLang 设计与实现文档

## 概述

MyLang 是一个用 OCaml 实现的简单函数式编程语言。本文档详细阐述其设计原理、实现步骤和依赖组件。

**核心特征：**
- 函数式编程范式（一等函数、词法作用域）
- 静态语法 + 动态类型检查
- 表达式导向（ everything is an expression ）
- 基于环境（environment）的变量绑定

---

## 架构总览

```
┌─────────────────────────────────────────────────────────────┐
│                         用户输入                              │
│              "let x = 10 in x + 5"                          │
└──────────────────────┬──────────────────────────────────────┘
                       │
                       ▼
┌─────────────────────────────────────────────────────────────┐
│  Lexer（词法分析器）                                          │
│  ─────────────────                                           │
│  输入：源代码字符串                                           │
│  输出：Token 列表                                             │
│  [LET, IDENT("x"), EQ, INT(10), IN, IDENT("x"), PLUS,       │
│   INT(5)]                                                    │
└──────────────────────┬──────────────────────────────────────┘
                       │
                       ▼
┌─────────────────────────────────────────────────────────────┐
│  Parser（语法分析器）                                         │
│  ──────────────────                                          │
│  输入：Token 列表                                             │
│  输出：AST（抽象语法树）                                       │
│  ELet("x", EInt 10, EAdd(EVar "x", EInt 5))                 │
└──────────────────────┬──────────────────────────────────────┘
                       │
                       ▼
┌─────────────────────────────────────────────────────────────┐
│  Evaluator（求值器）                                          │
│  ────────────────                                            │
│  输入：AST + 环境（变量绑定表）                                │
│  输出：Value（运行时值）                                       │
│  VInt 15                                                     │
└─────────────────────────────────────────────────────────────┘
```

---

## 实现步骤详解

### 第一步：抽象语法树（AST）设计

**文件：** `lib/ast.ml`

AST 是编译器/解释器的核心数据结构，它是对源代码的结构化表示，去除了所有语法噪声（空白、括号、关键字等），只保留语义信息。

**设计原则：**
1. **代数数据类型（Algebraic Data Types, ADT）**：OCaml 的 `type` 定义非常适合表达树形结构
2. **递归结构**：表达式可以包含子表达式，形成树
3. **区分表达式（expr）和值（value）**：表达式是未求值的代码，值是求值结果

```ocaml
type value =
  | VInt of int          (* 整数值：42 *)
  | VBool of bool        (* 布尔值：true/false *)
  | VFun of string * expr * env  (* 闭包：参数名 + 函数体 + 捕获的环境 *)
  | VUnit                (* 单元值：() *)
```

**为什么函数值（VFun）需要携带环境（env）？**

这就是**闭包（Closure）**的实现。函数定义时的环境被"捕获"并随函数一起保存，确保函数在调用时能访问定义时的变量，而不是调用时的变量。

```ocaml
let x = 10 in
let f = fun y -> x + y in   (* f 捕获了环境 [x -> 10] *)
let x = 20 in
f 5                         (* 结果仍是 15，不是 25 *)
```

### 第二步：词法分析（Lexical Analysis）

**文件：** `lib/lexer.mll`

**工具：** `ocamllex`（OCaml 的词法分析器生成器）

**原理：**
词法分析将字符流转换为 token 流。token 是语言的最小有意义单元（关键字、标识符、运算符、字面量等）。

**实现方式：**
`ocamllex` 使用正则表达式定义匹配规则，每个规则对应一个 token 类型。

```ocaml
rule read = parse
  | "let"    { LET }       (* 关键字 *)
  | "fun"    { FUN }       (* 关键字 *)
  | "+"      { PLUS }      (* 运算符 *)
  | digit+ as n { INT (int_of_string n) }  (* 整数字面量 *)
  | ident as s  { IDENT s } (* 标识符 *)
```

**关键概念：**
- **最长匹配原则**：`let` 匹配关键字，不是标识符 `l` + `e` + `t`
- **优先级**：规则按顺序匹配，前面的优先
- **状态机**：`ocamllex` 将正则表达式编译为确定性有限自动机（DFA），线性时间复杂度 O(n)

**错误处理：**
```ocaml
| _ { raise (SyntaxError ("Unexpected character: " ^ Lexing.lexeme lexbuf)) }
```
无法识别的字符触发语法错误。

### 第三步：语法分析（Syntax Analysis / Parsing）

**文件：** `lib/parser.mly`

**工具：** `menhir`（OCaml 的 LR(1) 解析器生成器）

**原理：**
语法分析将 token 流转换为 AST。它检查 token 的排列是否符合语言的语法规则。

**文法类型：**
MyLang 使用 **LR(1)** 文法，通过 `menhir` 自动生成解析表。

**BNF 文法（简化版）：**
```
expr ::= INT
       | BOOL
       | IDENT
       | expr PLUS expr
       | expr MINUS expr
       | expr STAR expr
       | expr SLASH expr
       | expr EQ expr
       | expr LT expr
       | expr AND expr
       | expr OR expr
       | NOT expr
       | IF expr THEN expr ELSE expr
       | LET IDENT EQ expr IN expr
       | FUN IDENT ARROW expr
       | expr expr              (* 函数应用 *)
       | LPAREN expr RPAREN
```

**优先级与结合性：**
```ocaml
%nonassoc IN
%right ARROW
%nonassoc ELSE
%left OR
%left AND
%nonassoc EQ NEQ LT LE GT GE
%left PLUS MINUS
%left STAR SLASH
%nonassoc NOT
```

这些声明解决文法歧义。例如 `1 + 2 * 3`：
- `*` 优先级高于 `+`，所以解析为 `1 + (2 * 3)`，不是 `(1 + 2) * 3`
- 所有运算符左结合，所以 `1 - 2 - 3` 解析为 `(1 - 2) - 3`

**移进/归约冲突（Shift/Reduce Conflict）：**

函数应用是隐式的（两个相邻表达式表示应用），这会产生歧义：
```ocaml
let f = fun x -> x + 1 in f 5
                          ^^^
                          这是 (f) (5) 还是变量名 "f5"？
```

`menhir` 通过优先级声明自动解决这类冲突（默认移进）。

### 第四步：求值器（Evaluator / Interpreter）

**文件：** `lib/eval.ml`

**实现策略：树遍历求值（Tree-walking Interpreter）**

求值器递归遍历 AST，根据节点类型执行相应操作。

**环境（Environment）模型：**

环境是变量名到值的关联列表（association list），形成作用域链：
```ocaml
type env = (string * value) list
```

**查找策略：** 从链表头部开始查找，第一个匹配即返回。这实现了**词法作用域**：内层绑定遮蔽（shadow）外层绑定。

```ocaml
let lookup env x =
  match List.assoc_opt x env with
  | Some v -> v
  | None -> raise (RuntimeError ("Unbound variable: " ^ x))
```

**各表达式类型的求值规则：**

| 表达式 | 求值规则 |
|--------|----------|
| `EInt n` | 直接返回 `VInt n` |
| `EBool b` | 直接返回 `VBool b` |
| `EVar x` | 在环境中查找 `x` |
| `EAdd(e1, e2)` | 先求值 `e1` 和 `e2`，要求都是 `VInt`，返回相加结果 |
| `ELet(x, e1, e2)` | 求值 `e1` 得 `v`，然后在 `[x -> v] :: env` 中求值 `e2` |
| `EFun(param, body)` | 返回闭包 `VFun(param, body, env)`，捕获当前环境 |
| `EApp(func, arg)` | 求值 `func` 得闭包，求值 `arg`，在闭包环境中绑定参数后求值函数体 |
| `EIf(cond, t, f)` | 求值 `cond`，`true` 则求值 `t`，`false` 则求值 `f` |

**惰性求值 vs 严格求值：**

MyLang 使用**严格求值（eager evaluation）**：函数参数先求值，再将值传入函数体。这与 OCaml、Rust、Python 等主流语言一致。

**短路逻辑：**

```ocaml
| EAnd (e1, e2) ->
    match eval env e1 with
    | VBool true -> eval env e2    (* 只有 e1 为 true 才求值 e2 *)
    | VBool false -> VBool false   (* e1 为 false 直接返回，不求值 e2 *)
```

这是常见的优化，避免不必要的计算和潜在错误（如 `false && (1 / 0)`）。

### 第五步：REPL（交互式解释器）

**文件：** `bin/main.ml`

REPL = Read（读取输入）→ Eval（求值）→ Print（打印结果）→ Loop（循环）

实现了一个简单的命令行界面：
- 读取用户输入的一行代码
- 调用 `My_lang.run_exn` 进行解析和求值
- 打印结果或错误信息
- 支持从文件加载代码执行

**错误处理：**
```ocaml
let run_exn s =
  match run s with
  | v -> Ok v
  | exception Lexer.SyntaxError msg -> Error ("Syntax error: " ^ msg)
  | exception Parser.Error -> Error "Parse error"
  | exception Eval.RuntimeError msg -> Error ("Runtime error: " ^ msg)
```

使用 OCaml 的异常处理捕获各阶段的错误，统一包装为 `Result` 类型。

---

## 依赖组件

### 1. OCaml 编译器（`ocaml`）
- **版本**：4.14.1
- **作用**：编译 OCaml 源码
- **来源**：Ubuntu apt 包或 opam 安装

### 2. opam（OCaml Package Manager）
- **版本**：2.1.5
- **作用**：管理 OCaml 库依赖、编译器版本（switch）
- **初始化**：`opam init`

### 3. dune（构建系统）
- **版本**：3.23.1
- **作用**：OCaml 的现代化构建工具，替代传统的 Makefile/OCamlBuild
- **功能**：
  - 自动发现依赖关系
  - 增量编译
  - 库和可执行文件的定义
  - 测试运行

**dune 文件示例：**
```scheme
(library
 (name my_lang)
 (libraries core))

(menhir (modules parser))
(ocamllex lexer)
```

### 4. menhir（解析器生成器）
- **版本**：20260209
- **作用**：将 `.mly` 文法文件编译为 OCaml 解析器
- **算法**：LR(1) / LALR
- **对比**：
  - `ocamlyacc`：OCaml 自带的 yacc，功能较旧
  - `menhir`：功能更强大，错误报告更好，支持增量解析

### 5. ocamllex（词法分析器生成器）
- **内置工具**：随 OCaml 编译器附带
- **作用**：将 `.mll` 规则文件编译为 OCaml 词法分析器
- **算法**：基于正则表达式的 DFA（确定性有限自动机）

### 6. Core（标准库扩展）
- **版本**：v0.16.2
- **来源**：Jane Street
- **作用**：提供更现代、一致的 OCaml 标准库
- **特性**：
  - 更丰富的数据结构和算法
  - 统一的命名约定
  - 改进的 I/O 模块（`In_channel`, `Out_channel`）

**注意：** Core 与 OCaml 标准库部分不兼容，使用 Core 后应避免混用 `Stdlib` 模块。

### 7. ppx_jane（语法扩展）
- **版本**：v0.16.0
- **来源**：Jane Street
- **作用**：提供各种 PPX（PreProcessor eXtension）
- **用途**：在我们的测试中用于 `let%test`（内联测试），虽然最终改为传统测试方式

### 8. Merlin（IDE 支持）
- **版本**：4.19
- **作用**：为编辑器提供代码补全、类型提示、跳转到定义
- **集成**：VS Code（OCaml Platform 插件）、Vim、Emacs

### 9. ocaml-lsp-server（LSP 服务器）
- **版本**：1.21.0
- **作用**：Language Server Protocol 实现，为 IDE 提供标准化接口
- **功能**：自动补全、类型标注、代码重构、错误诊断

---

## 关键技术原理

### 1. 闭包（Closure）

闭包是函数式编程的核心概念。它由**函数代码**和**定义时的环境**组成。

**实现：**
```ocaml
| EFun (param, body) -> VFun (param, body, env)
```

求值 `EFun` 时，当前环境 `env` 被捕获进 `VFun`。

**调用时的环境构建：**
```ocaml
| EApp (func, arg) ->
    let func_val = eval env func in
    let arg_val = eval env arg in
    (match func_val with
     | VFun (param, body, closure_env) ->
         eval ((param, arg_val) :: closure_env) body
     | _ -> raise ...)
```

关键点：在**闭包环境**（`closure_env`）中扩展参数绑定，不是在调用时的环境。这保证了词法作用域。

### 2. 高阶函数

函数可以作为参数和返回值：
```ocaml
let apply = fun f -> fun x -> f x in
let add1 = fun y -> y + 1 in
apply add1 5   (* => 6 *)
```

求值过程：
1. `apply` 求值为 `VFun("f", EFun("x", EApp(EVar "f", EVar "x")), [])`
2. `add1` 求值为 `VFun("y", EAdd(EVar "y", EInt 1), [])`
3. `apply add1` 求值：将 `f` 绑定到 `add1` 的闭包，返回 `VFun("x", ..., [f -> add1闭包])`
4. `(... ) 5` 求值：将 `x` 绑定到 5，在环境 `[x->5, f->add1闭包]` 中求值 `f x`

### 3. 柯里化（Currying）

多参数函数通过返回函数实现：
```ocaml
fun x -> fun y -> x + y
```

这是 `fun x -> (fun y -> x + y)`，不是 `(fun x -> fun y) -> x + y`。

调用：`add 3 4` 等价于 `(add 3) 4`
1. `add 3` 返回一个函数（`x` 已绑定为 3）
2. 该函数再应用到 `4`，得到 `3 + 4 = 7`

### 4. 递归与不动点（未来扩展）

当前 MyLang 不支持直接递归（没有 `let rec`）。要实现递归，需要**不动点组合子（Y Combinator）**：

```ocaml
let fix = fun f ->
  (fun x -> f (x x)) (fun x -> f (x x))
in
let factorial = fix (fun f -> fun n ->
  if n = 0 then 1 else n * f (n - 1))
in factorial 5
```

或者更简单的方式：在 AST 中增加 `let rec` 节点，让求值器在绑定变量时允许引用自身。

### 5. 类型系统（未来扩展）

当前 MyLang 是**动态类型**的：类型检查在运行时进行（如 `EAdd` 要求两个操作数都是 `VInt`）。

要实现**静态类型系统**（如 Hindley-Milner 类型推断）：
1. 为每个表达式添加类型标注（或推导）
2. 收集类型约束方程
3. 使用统一（unification）算法求解约束
4. 在编译期（解析后）检查类型错误

OCaml 自身的类型系统就是 Hindley-Milner 的实现，是学习的好范本。

---

## 扩展路线图

### Phase 1：语言核心（已完成）
- [x] 整数、布尔值
- [x] 算术/逻辑/比较运算符
- [x] let 绑定
- [x] 函数定义与应用
- [x] if-then-else
- [x] REPL

### Phase 2：数据类型（已完成）
- [x] 列表（List）：`[1, 2, 3]`，`head :: tail`
- [x] 元组（Tuple）：`(1, true, "hello")`
- [x] 字符串（String）
- [x] 单元（Unit）：`()`

### Phase 3：控制流（已完成）
- [x] `let rec` 递归绑定
- [x] 顺序执行（`e1; e2`）
- [x] 模式匹配（Pattern Matching）
- [x] 内置函数（head, tail, length, print）

### Phase 4：模块系统（已完成）
- [x] 多文件项目支持（`import "file.ml"`）
- [x] 类型推断中处理 import 绑定
- [ ] 模块签名（Signature）
- [ ] 命名空间/qualified import

### Phase 5：类型系统（已完成）
- [x] Hindley-Milner 类型推断
- [x] let 多态性
- [x] 内置函数多态类型
- [ ] 基本类型标注（`: int`, `: bool`）
- [ ] 代数数据类型（ADT）
- [ ] 参数化多态显式语法（`'a list`）

### Phase 6：编译器后端
- [ ] 字节码（Bytecode）编译器
- [ ] 虚拟机（VM）执行
- [ ] LLVM IR 生成

---

## 参考资源

### 书籍
- **《Modern Compiler Implementation in ML》**（Andrew W. Appel）— 用 ML 实现编译器的经典教材
- **《Types and Programming Languages》**（Benjamin C. Pierce）— 类型系统权威参考书

### 在线资源
- [OCaml 官方文档](https://ocaml.org/docs)
- [Menhir 手册](http://gallium.inria.fr/~fpottier/menhir/manual.html)
- [Real World OCaml](https://dev.realworldocaml.org/) — OCaml 实战
- [Crafting Interpreters](https://craftinginterpreters.com/) — 用 Java 和 C 实现解释器，概念通用

### 相关项目
- **MiniML** — 多个教学用 ML 子集实现
- **OCaml 编译器源码** — 位于 `ocaml/compiler-libs/`
- **Rustboot** — Rust 的 OCaml 原型编译器（已归档）

---

## 性能考量

### 当前实现的复杂度

| 操作 | 时间复杂度 | 说明 |
|------|----------|------|
| 词法分析 | O(n) | DFA，线性扫描 |
| 语法分析 | O(n) | LR(1)，线性时间 |
| 环境查找 | O(d) | d = 作用域深度，链表查找 |
| 函数调用 | O(1) | 环境扩展是 cons 操作 |

### 优化方向

1. **环境表示**：当前使用关联列表，深度作用域时查找为 O(n)。可改用哈希表或数组 + 栈帧指针。
2. **尾调用优化（TCO）**：当前递归调用会增长调用栈。应实现尾调用复用当前栈帧。
3. **编译为字节码**：树遍历解释器开销大，编译为字节码后由 VM 执行可提升 10-100 倍。

---

*文档版本：1.0*
*最后更新：2026-05-25*
