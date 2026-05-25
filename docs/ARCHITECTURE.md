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

### 字节码执行模式

```
Source Code → Lexer → Parser → AST → Compiler → Bytecode → VM → Value
     (string)  (tokens)  (tree)  (expr)   (code)     (instr)   (result)
```

**组件：**
- **Compiler** (`lib/compiler.ml`)：将 AST 编译为字节码指令序列
- **Bytecode** (`lib/bytecode.ml`)：定义指令集（PUSH, ADD, CALL, JUMP 等）
- **VM** (`lib/vm.ml`)：基于栈的虚拟机，执行字节码指令

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
  | VInt of int              (* 整数值：42 *)
  | VBool of bool            (* 布尔值：true/false *)
  | VString of string        (* 字符串值："hello" *)
  | VList of value list      (* 列表值：[1; 2; 3] *)
  | VTuple of value list     (* 元组值：(1, true, "hello") *)
  | VFun of string option * string * expr * env
                             (* 闭包：可选递归名 + 参数 + 函数体 + 捕获的环境 *)
  | VBuiltin of string * (env -> value -> value * env)
                             (* 内置函数：名称 + 实现 *)
  | VUnit                    (* 单元值：() *)
```

**表达式类型：**

```ocaml
type expr =
  | EInt of int              (* 整数字面量 *)
  | EBool of bool            (* 布尔字面量 *)
  | EString of string        (* 字符串字面量 *)
  | EList of expr list       (* 列表字面量 [1, 2, 3] *)
  | ETuple of expr list      (* 元组字面量 (1, 2) *)
  | EVar of string           (* 变量引用 *)
  | EAdd of expr * expr      (* 加法 e1 + e2 *)
  | ESub of expr * expr      (* 减法 e1 - e2 *)
  | EMul of expr * expr      (* 乘法 e1 * e2 *)
  | EDiv of expr * expr      (* 除法 e1 / e2 *)
  | EEq of expr * expr       (* 等于 e1 = e2 *)
  | ENeq of expr * expr      (* 不等于 e1 <> e2 *)
  | ELt of expr * expr       (* 小于 e1 < e2 *)
  | ELe of expr * expr       (* 小于等于 e1 <= e2 *)
  | EGt of expr * expr       (* 大于 e1 > e2 *)
  | EGe of expr * expr       (* 大于等于 e1 >= e2 *)
  | EAnd of expr * expr      (* 逻辑与 e1 && e2 *)
  | EOr of expr * expr       (* 逻辑或 e1 || e2 *)
  | ENot of expr             (* 逻辑非 not e *)
  | EIf of expr * expr * expr   (* 条件 if c then t else f *)
  | ELet of string * expr * expr  (* 变量绑定 let x = e1 in e2 *)
  | ELetRec of string * expr * expr  (* 递归绑定 let rec f = e1 in e2 *)
  | EFun of string * expr    (* 匿名函数 fun x -> e *)
  | EApp of expr * expr      (* 函数应用 e1 e2 *)
  | ECons of expr * expr     (* 列表构造 e1 :: e2 *)
  | ECat of expr * expr      (* 字符串拼接 e1 ^ e2 *)
  | EMatch of expr * (pattern * expr) list  (* 模式匹配 *)
  | ESeq of expr * expr      (* 顺序执行 e1; e2 *)
```

**模式类型：**

```ocaml
type pattern =
  | PWildcard                (* 通配符 _ *)
  | PVar of string           (* 变量模式 x *)
  | PInt of int              (* 整数模式 42 *)
  | PBool of bool            (* 布尔模式 true/false *)
  | PString of string        (* 字符串模式 "hello" *)
  | PUnit                    (* 单元模式 () *)
  | PList of pattern list    (* 列表模式 [p1, p2] *)
  | PTuple of pattern list   (* 元组模式 (p1, p2) *)
  | PCons of pattern * pattern  (* cons 模式 h :: t *)
```

**为什么函数值（VFun）需要携带环境（env）？**

这就是**闭包（Closure）**的实现。函数定义时的环境被"捕获"并随函数一起保存，确保函数在调用时能访问定义时的变量，而不是调用时的变量。

```ocaml
let x = 10 in
let f = fun y -> x + y in   (* f 捕获了环境 [x -> 10] *)
let x = 20 in
f 5                         (* 结果仍是 15，不是 25 *)
```

**递归闭包：**

`let rec` 绑定通过 `VFun` 的 `string option` 参数实现自引用。在绑定函数名时，将函数自身加入其捕获的环境：

```ocaml
| ELetRec (f, EFun (param, body), rest) ->
    let rec env' = (f, VFun (Some f, param, body, env')) :: env in
    eval env' rest
```

当调用时检测到 `Some f`，将函数自身绑定到参数环境中，实现递归调用。

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
| `EString s` | 直接返回 `VString s` |
| `EList es` | 依次求值各元素，返回 `VList [v1; v2; ...]` |
| `ETuple es` | 依次求值各元素，返回 `VTuple [v1; v2; ...]` |
| `EVar x` | 在环境中查找 `x` |
| `EAdd(e1, e2)` | 先求值 `e1` 和 `e2`，要求都是 `VInt`，返回相加结果 |
| `ESub(e1, e2)` | 整数减法 |
| `EMul(e1, e2)` | 整数乘法 |
| `EDiv(e1, e2)` | 整数除法，检查除零 |
| `EEq(e1, e2)` | 比较相等，支持 int/bool/string/unit |
| `ENeq(e1, e2)` | 比较不等 |
| `ELt(e1, e2)` | 小于，支持 int/string |
| `ELe(e1, e2)` | 小于等于 |
| `EGt(e1, e2)` | 大于 |
| `EGe(e1, e2)` | 大于等于 |
| `EAnd(e1, e2)` | 逻辑与，短路求值 |
| `EOr(e1, e2)` | 逻辑或，短路求值 |
| `ENot e` | 逻辑非 |
| `EIf(cond, t, f)` | 求值 `cond`，`true` 则求值 `t`，`false` 则求值 `f` |
| `ELet(x, e1, e2)` | 求值 `e1` 得 `v`，然后在 `[x -> v] :: env` 中求值 `e2` |
| `ELetRec(f, e1, e2)` | 求值 `e1`（应为函数），创建自引用环境，求值 `e2` |
| `EFun(param, body)` | 返回闭包 `VFun(None, param, body, env)`，捕获当前环境 |
| `EApp(func, arg)` | 求值 `func` 得闭包，求值 `arg`，在闭包环境中绑定参数后求值函数体 |
| `ECons(e1, e2)` | 求值 `e1` 得 `v1`，求值 `e2` 得 `VList vs`，返回 `VList (v1 :: vs)` |
| `ECat(e1, e2)` | 求值 `e1` 和 `e2`，要求都是 `VString`，返回拼接结果 |
| `EMatch(e, cases)` | 求值 `e` 得 `v`，依次尝试各模式，第一个匹配成功的分支求值 |
| `ESeq(e1, e2)` | 求值 `e1`，忽略结果，在更新后的环境中求值 `e2` |

**模式匹配求值：**

```ocaml
let rec match_pattern pat value =
  match pat, value with
  | PWildcard, _ -> Some []
  | PVar x, v -> Some [(x, v)]
  | PInt n, VInt m when n = m -> Some []
  | PBool b, VBool c when b = c -> Some []
  | PString s, VString t when s = t -> Some []
  | PUnit, VUnit -> Some []
  | PList ps, VList vs when List.length ps = List.length vs ->
      match_patterns ps vs
  | PTuple ps, VTuple vs when List.length ps = List.length vs ->
      match_patterns ps vs
  | PCons (p1, p2), VList (h :: t) ->
      (match match_pattern p1 h with
       | Some b1 ->
           (match match_pattern p2 (VList t) with
            | Some b2 -> Some (b1 @ b2)
            | None -> None)
       | None -> None)
  | _ -> None
```

模式匹配返回 `Some bindings`（变量绑定列表）或 `None`（不匹配）。`bindings` 被追加到当前环境后求值对应分支。

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

### 第五步：字节码编译器与虚拟机

MyLang 已实现树遍历解释器之外的**字节码编译执行**模式，提供更高性能。

**执行流程：**
```
源代码 → 词法分析 → 语法分析 → AST → 编译器 → 字节码 → VM → 结果
```

**字节码指令集（`lib/bytecode.ml`）：**

```ocaml
type instr =
  (* 常量 *)
  | PushInt of int
  | PushBool of bool
  | PushString of string
  | PushUnit
  | PushNil
  (* 变量 *)
  | LoadVar of string
  | StoreVar of string
  (* 算术 *)
  | Add | Sub | Mul | Div
  (* 比较 *)
  | Eq | Neq | Lt | Le | Gt | Ge
  (* 逻辑 *)
  | And | Or | Not
  (* 控制流 *)
  | Jump of int
  | JumpIfFalse of int
  (* 函数 *)
  | MakeClosure of string * code * string option
  | Call
  | Return
  (* 列表 *)
  | MakeList of int
  | Cons | Head | Tail | Length
  (* 字符串 *)
  | Concat
  (* 其他 *)
  | Print | Pop | Dup
```

**编译器（`lib/compiler.ml`）：**

采用**回填技术（backpatching）**处理控制流：
- `if` 编译时先占位跳转地址，待 `else` 和 `end` 位置确定后修补
- 指令累积在列表中（O(1) 尾部追加），最后反转并转为数组

**虚拟机（`lib/vm.ml`）：**

基于栈的虚拟机，核心状态：
- `stack`：操作栈（值列表）
- `env`：当前环境（变量绑定列表）
- `pc`：程序计数器
- `call_stack`：调用栈（保存返回地址、调用者栈、调用者环境）

函数调用时保存调用者状态，创建新栈和参数环境，递归执行函数体。
返回时恢复调用者状态。对于最外层返回，使用局部异常 `ReturnExn` 跳出执行循环。

### 第六步：REPL（交互式解释器）

**文件：** `bin/main.ml`

REPL = Read（读取输入）→ Eval（求值）→ Print（打印结果）→ Loop（循环）

实现了一个简单的命令行界面：
- 读取用户输入的一行代码
- 调用 `My_lang.run_exn` 进行解析、类型检查和求值
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
  | exception Types.TypeError msg -> Error ("Type error: " ^ msg)
  | exception Vm.VMError msg -> Error ("VM error: " ^ msg)
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

### 4. 递归

MyLang 支持 `let rec` 直接递归绑定：

```ocaml
let rec factorial = fun n ->
  if n = 0 then 1 else n * factorial (n - 1)
in factorial 5   (* => 120 *)
```

实现方式：在求值 `let rec` 时，将函数值自身绑定到函数名，形成自引用环境：

```ocaml
| ELetRec (f, EFun (param, body), rest) ->
    let rec env' = (f, VFun (Some f, param, body, env')) :: env in
    eval env' rest
```

`VFun` 的第一个参数 `string option` 为 `Some f` 时，调用时会将函数自身加入参数环境。

### 5. 类型系统

MyLang 已实现 **Hindley-Milner 类型推断** 和 **let-多态性**。

**类型表示：**

```ocaml
type t =
  | TInt
  | TBool
  | TString
  | TUnit
  | TList of t
  | TTuple of t list
  | TArrow of t * t   (* 函数类型 t1 -> t2 *)
  | TVar of int       (* 类型变量 *)

type scheme = Forall of int list * t   (* 多态类型 ∀α.t *)
```

**核心算法：**

1. **类型推断（Infer）**：为每个表达式生成类型，引入类型变量表示未知类型
2. **统一（Unification）**：比较两个类型，生成替换使它们相等
3. **Occurs Check**：防止构造循环类型（如 `t = t list`）
4. **泛化（Generalization）**：将 `let` 绑定的自由变量转为多态变量
5. **实例化（Instantiation）**：为每次函数调用生成新的类型变量

**示例：**

```ocaml
let id = fun x -> x in
(id 5, id true)
```

`id` 被推断为 `'a -> 'a`，然后泛化为 `∀'a. 'a -> 'a`。在 `id 5` 中实例化为 `int -> int`，在 `id true` 中实例化为 `bool -> bool`。

**错误检测：**

```ocaml
"hello" + 1   (* Type error: cannot unify string with int *)
```

类型检查在求值前执行，提前捕获类型错误。

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
- [x] while 循环（`while cond do body done`）
- [x] 索引访问（`list[0]`，`string[0]`）
- [x] 内置函数（head, tail, length, print）

### Phase 4：模块系统（已完成）
- [x] 多文件项目支持（`import "file.ml"`）
- [x] 类型推断中处理 import 绑定
- [ ] 模块签名（Signature）
- [ ] 命名空间/qualified import
- [ ] 包管理器

### Phase 5：类型系统（已完成）
- [x] Hindley-Milner 类型推断
- [x] let 多态性
- [x] 内置函数多态类型
- [ ] 基本类型标注（`: int`, `: bool`）
- [ ] 代数数据类型（ADT）
- [ ] 参数化多态显式语法（`'a list`）
- [ ] 类型错误位置报告（行号/列号）

### Phase 6：编译器后端（已完成）
- [x] 字节码（Bytecode）编译器
- [x] 虚拟机（VM）执行
- [x] 尾调用优化（TCO）：窥孔优化将 `Call + Return` 替换为 `TailCall`
- [ ] 垃圾回收
- [ ] JIT 编译
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
| 类型推断 | O(n) | Hindley-Milner，接近线性 |
| 环境查找 | O(d) | d = 作用域深度，链表查找 |
| 函数调用 | O(1) | 环境扩展是 cons 操作 |
| 字节码执行 | O(n) | 每条指令 O(1)，循环执行 |

### 已完成的优化

1. **类型替换**：使用 `Int.Map` 替换列表实现，从 O(n) 提升到 O(log n)
2. **指令累积**：编译器从 `Array.append` 改为列表累积，避免 O(n²) 开销
3. **VM 状态**：使用 mutable refs 减少 GC 压力
4. **尾调用优化（TCO）**：通过窥孔优化将 `Call + Return` 替换为 `TailCall`，复用当前栈帧，避免递归调用栈无限增长

### 未来优化方向

1. **环境表示**：当前使用关联列表，深度作用域时查找为 O(n)。可改用哈希表或数组 + 栈帧指针。
2. **JIT 编译**：热点函数编译为机器码执行。

---

## 开发资源

### 最佳实践
- **`claude.md`** — 开发最佳实践文档，包含：
  - VM 设计原则
  - 调试技巧
  - 常见 Bug 模式
  - 编译器开发规范
  - 测试策略
  - Git 工作流

---

*文档版本：1.2*
*最后更新：2026-05-25*
