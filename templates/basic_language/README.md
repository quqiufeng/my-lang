# 基础语言模板

这是一个用于创建新编程语言的模板项目。

## 快速开始

```bash
# 复制模板
cp -r templates/basic_language my_language
cd my_language

# 构建
dune build

# 运行 REPL
dune exec bin/main.exe

# 运行文件
echo "let x = 42 in x + 1" > test.ml
dune exec bin/main.exe test.ml
```

## 项目结构

```
my_language/
├── lib/
│   ├── ast.ml          # AST 定义
│   ├── lexer.mll       # 词法分析器
│   ├── parser.mly      # 语法分析器
│   ├── eval.ml         # 求值器
│   └── my_lang.ml      # 库入口
├── bin/
│   └── main.ml         # CLI 入口
├── dune-project        # Dune 项目配置
└── README.md           # 本文档
```

## 如何扩展

### 1. 添加新的语法

在 `parser.mly` 中添加新的语法规则：

```ocaml
(* 添加 while 循环 *)
while_expr:
  | WHILE; cond = expr; DO; body = expr; DONE
    { EWhile (cond, body) }
```

### 2. 添加新的 AST 节点

在 `ast.ml` 中添加新的表达式类型：

```ocaml
type expr =
  (* ... 现有表达式 ... *)
  | EWhile of expr * expr  (* while cond do body done *)
```

### 3. 添加新的求值逻辑

在 `eval.ml` 中添加新的求值规则：

```ocaml
let rec eval env = function
  (* ... 现有规则 ... *)
  | EWhile (cond, body) ->
      let rec loop () =
        match eval env cond with
        | VBool true ->
            let _ = eval env body in
            loop ()
        | VBool false -> VUnit
        | _ -> raise (RuntimeError "Condition must be boolean")
      in
      loop ()
```

### 4. 添加新的内置函数

在 `eval.ml` 的 `builtin_env` 中添加：

```ocaml
let builtin_env = [
  (* ... 现有内置函数 ... *)
  ("sqrt", VBuiltin ("sqrt", fun args ->
    match args with
    | [VInt n] -> VInt (int_of_float (sqrt (float_of_int n)))
    | _ -> raise (RuntimeError "sqrt expects an integer")));
]
```

## 示例语言特性

### 基本语法

```ocaml
(* 整数 *)
42

(* 布尔值 *)
true
false

(* 字符串 *)
"hello"

(* 变量 *)
let x = 42 in x + 1

(* 函数 *)
let add = fun a b -> a + b in add 1 2

(* 条件 *)
if true then 1 else 2

(* 模式匹配 *)
match [1, 2, 3] with
| [] -> 0
| x :: rest -> x

(* 元组 *)
(1, "hello", true)

(* 列表 *)
[1, 2, 3]
```

## 下一步

1. 添加类型系统
2. 添加更多内置函数
3. 添加错误处理
4. 添加模块系统
5. 添加编译器后端

## 参考

- [OCaml 编程语言实现最佳实践](../../docs/BEST_PRACTICES_GUIDE.md)
- [MyLang 架构文档](../../docs/ARCHITECTURE.md)
- [MyLang 开发指南](../../docs/CONTRIBUTING.md)
