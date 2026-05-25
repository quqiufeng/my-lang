# 从零开始实现编程语言（使用 OCaml）

本教程将带你了解 MyLang 的实现过程，帮助你理解如何用 OCaml 构建自己的编程语言。

## 目录

1. [第一阶段：计算器](#第一阶段计算器)
2. [第二阶段：变量和函数](#第二阶段变量和函数)
3. [第三阶段：类型系统](#第三阶段类型系统)
4. [第四阶段：编译器后端](#第四阶段编译器后端)
5. [第五阶段：高级特性](#第五阶段高级特性)

---

## 第一阶段：计算器

最简单的编程语言是只能计算算术表达式的计算器。

### AST 定义

```ocaml
type expr =
  | EInt of int
  | EAdd of expr * expr
  | ESub of expr * expr
```

### 求值器

```ocaml
let rec eval = function
  | EInt n -> n
  | EAdd (e1, e2) -> eval e1 + eval e2
  | ESub (e1, e2) -> eval e1 - eval e2
```

### 测试

```ocaml
let expr = EAdd (EInt 1, EInt 2)  (* 1 + 2 *)
let result = eval expr             (* => 3 *)
```

**练习**：添加乘法和除法。

---

## 第二阶段：变量和函数

让语言支持变量绑定和函数定义。

### AST 扩展

```ocaml
type expr =
  | EInt of int
  | EAdd of expr * expr
  | EVar of string           (* 变量引用 *)
  | ELet of string * expr * expr  (* let x = e1 in e2 *)
  | EFun of string * expr    (* fun x -> e *)
  | EApp of expr * expr      (* 函数应用 *)
```

### 环境模型

使用关联列表表示变量绑定：

```ocaml
type env = (string * value) list

type value =
  | VInt of int
  | VFun of string * expr * env  (* 闭包：参数 + 函数体 + 环境 *)
```

### 闭包

函数定义时捕获当前环境：

```ocaml
let x = 10 in
let f = fun y -> x + y in   (* f 捕获了 [x -> 10] *)
let x = 20 in
f 5                           (* 结果仍是 15 *)
```

**练习**：实现 `let rec` 递归绑定。

---

## 第三阶段：类型系统

添加 Hindley-Milner 类型推断，让编译器自动推断类型。

### 类型表示

```ocaml
type t =
  | TInt
  | TBool
  | TString
  | TArrow of t * t   (* t1 -> t2 *)
  | TVar of int       (* 类型变量 *)
```

### 核心算法：统一（Unification）

统一两个类型，生成替换：

```ocaml
let rec unify t1 t2 =
  match t1, t2 with
  | TInt, TInt -> Subst.empty
  | TVar n, t | t, TVar n -> Subst.singleton n t
  | TArrow (a1, a2), TArrow (b1, b2) ->
      let s1 = unify a1 b1 in
      let s2 = unify (apply s1 a2) (apply s1 b2) in
      compose s2 s1
  | _ -> raise (TypeError "cannot unify")
```

### let-多态性

`let id = fun x -> x in (id 5, id true)` 中，`id` 获得类型 `'a -> 'a`。

**练习**：实现列表类型 `TList of t`。

---

## 第四阶段：编译器后端

将 AST 编译为字节码，用虚拟机执行。

### 字节码指令

```ocaml
type instr =
  | PushInt of int
  | Add
  | LoadVar of string
  | StoreVar of string
  | Jump of int
  | JumpIfFalse of int
  | Call
  | Return
```

### 编译示例

```ocaml
(* let x = 1 + 2 in x + 3 *)
[
  PushInt 1; PushInt 2; Add;       (* 计算 1 + 2 *)
  StoreVar "x";                     (* x = 3 *)
  LoadVar "x"; PushInt 3; Add;     (* x + 3 *)
  Return
]
```

### 虚拟机

基于栈的虚拟机：

```ocaml
let run code =
  let stack = ref [] in
  let env = ref [] in
  let pc = ref 0 in
  
  while !pc < Array.length code do
    match code.(!pc) with
    | PushInt n -> stack := VInt n :: !stack
    | Add ->
        (match !stack with
         | VInt b :: VInt a :: rest ->
             stack := VInt (a + b) :: rest)
    | ...
  done
```

**练习**：实现尾调用优化。

---

## 第五阶段：高级特性

### 模式匹配

```ocaml
match xs with
| [] -> 0
| h :: t -> h + sum t
```

实现方式：依次尝试每个模式，第一个匹配的绑定变量并执行对应分支。

### 高阶函数

```ocaml
map (fun x -> x + 1) [1, 2, 3]
```

内置 `map`、`filter`、`fold` 函数，接受函数作为参数。

### 模块系统

```ocaml
import "stdlib/list.ml"
```

读取文件内容，解析为 AST，提取绑定并加入当前环境。

---

## 总结

构建编程语言的核心步骤：

1. **定义 AST** — 你的语言能表达什么
2. **词法/语法分析** — 将文本转为 AST
3. **类型检查** — 确保程序正确（可选但推荐）
4. **求值/编译** — 执行程序
5. **迭代扩展** — 添加新特性

每个阶段都是独立的，可以单独测试和优化。

## 进一步阅读

- [docs/ARCHITECTURE.md](ARCHITECTURE.md) — MyLang 的完整架构设计
- [docs/CONTRIBUTING.md](CONTRIBUTING.md) — 如何添加新特性
- 《Types and Programming Languages》(Benjamin Pierce) — 类型系统权威教材
- 《Modern Compiler Implementation in ML》(Andrew Appel) — 编译器实现
