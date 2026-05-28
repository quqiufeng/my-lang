# 从零开始创建一门编程语言

本教程将指导你使用 MyLang 框架从零开始创建一门新的编程语言。

## 前置条件

- OCaml 4.14+
- Dune 构建系统
- Menhir 解析器生成器

## 第一步：创建项目

```bash
# 复制模板
cp -r templates/basic_language my_language
cd my_language

# 构建
dune build
```

## 第二步：理解 AST

AST（抽象语法树）是编译器/解释器的核心数据结构。它表示源代码的结构化表示。

```ocaml
(* ast.ml *)
type expr =
  | EInt of int           (* 整数：42 *)
  | EBool of bool         (* 布尔：true *)
  | EVar of string        (* 变量：x *)
  | ELet of string * expr * expr  (* let x = 1 in x *)
  | EFun of string list * expr    (* fun x -> x + 1 *)
  | EApp of expr * expr list      (* f(1, 2) *)
  | EIf of expr * expr * expr     (* if cond then e1 else e2 *)
  | EBinary of binary_op * expr * expr  (* 1 + 2 *)
```

## 第三步：实现词法分析器

词法分析器将源代码字符串转换为 Token 列表。

```ocaml
(* lexer.mll *)
rule read = parse
  | [' ' '\t' '\n'] { read lexbuf }  (* 跳过空白 *)
  | ['0'-'9']+ { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | "let" { LET }
  | "in" { IN }
  | ['a'-'z' 'A'-'Z' '_']+ { IDENT (Lexing.lexeme lexbuf) }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { STAR }
  | '/' { SLASH }
  | eof { EOF }
```

## 第四步：实现语法分析器

语法分析器将 Token 列表转换为 AST。

```ocaml
(* parser.mly *)
%token <int> INT
%token <string> IDENT
%token LET IN
%token PLUS MINUS STAR SLASH
%token EOF

%left PLUS MINUS
%left STAR SLASH

%start <Ast.expr> prog

%%

prog:
  | e = expr; EOF { e }

expr:
  | e = add_expr { e }

add_expr:
  | e1 = add_expr; PLUS; e2 = mul_expr { EBinary (Add, e1, e2) }
  | e1 = add_expr; MINUS; e2 = mul_expr { EBinary (Sub, e1, e2) }
  | e = mul_expr { e }

mul_expr:
  | e1 = mul_expr; STAR; e2 = primary { EBinary (Mul, e1, e2) }
  | e1 = mul_expr; SLASH; e2 = primary { EBinary (Div, e1, e2) }
  | e = primary { e }

primary:
  | n = INT { EInt n }
  | x = IDENT { EVar x }
  | LPAREN; e = expr; RPAREN { e }
```

## 第五步：实现求值器

求值器将 AST 转换为值。

```ocaml
(* eval.ml *)
let rec eval env = function
  | EInt n -> VInt n
  | EVar x ->
      (match List.assoc_opt x env with
       | Some v -> v
       | None -> raise (RuntimeError ("Unbound variable: " ^ x)))
  | ELet (x, e1, e2) ->
      let v1 = eval env e1 in
      eval ((x, v1) :: env) e2
  | EBinary (op, e1, e2) ->
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      eval_binary_op op v1 v2
```

## 第六步：添加新功能

### 添加字符串支持

1. 在 `ast.ml` 中添加字符串类型：
```ocaml
type expr =
  (* ... *)
  | EString of string
```

2. 在 `lexer.mll` 中添加字符串词法：
```ocaml
| '"' { string (Buffer.create 16) lexbuf }
```

3. 在 `parser.mly` 中添加字符串语法：
```ocaml
| s = STRING { EString s }
```

4. 在 `eval.ml` 中添加字符串求值：
```ocaml
| EString s -> VString s
```

### 添加函数调用

1. 在 `ast.ml` 中添加函数类型：
```ocaml
type value =
  (* ... *)
  | VFun of string list * expr * env
```

2. 在 `eval.ml` 中添加函数求值：
```ocaml
| EFun (params, body) -> VFun (params, body, env)
| EApp (f, args) ->
    let fv = eval env f in
    let argvs = List.map (eval env) args in
    apply_value fv argvs
```

### 添加模式匹配

1. 在 `ast.ml` 中添加模式类型：
```ocaml
type pattern =
  | PVar of string
  | PInt of int
  | PWild
```

2. 在 `eval.ml` 中添加模式匹配求值：
```ocaml
| EMatch (e, cases) ->
    let v = eval env e in
    eval_match env v cases
```

## 第七步：测试

```bash
# 运行测试
dune test

# 运行示例
echo "let x = 42 in x + 1" > test.ml
dune exec bin/main.exe test.ml
```

## 第八步：添加更多功能

### 添加类型系统

```ocaml
(* typeinfer.ml *)
type typ =
  | TInt
  | TBool
  | TString
  | TArrow of typ * typ
  | TVar of string

let rec infer env = function
  | EInt _ -> TInt
  | EBool _ -> TBool
  | EString _ -> TString
  | EBinary (Add, e1, e2) ->
      let t1 = infer env e1 in
      let t2 = infer env e2 in
      unify t1 TInt;
      unify t2 TInt;
      TInt
```

### 添加编译器后端

```ocaml
(* compiler.ml *)
type instruction =
  | PUSH of value
  | LOAD of string
  | STORE of string
  | ADD | SUB | MUL | DIV
  | CALL of int
  | RETURN

let rec compile = function
  | EInt n -> [PUSH (VInt n)]
  | EVar x -> [LOAD x]
  | EBinary (op, e1, e2) ->
      compile e1 @ compile e2 @ [compile_op op]
```

## 总结

通过本教程，你已经学会了：

1. 如何设计 AST
2. 如何实现词法分析器
3. 如何实现语法分析器
4. 如何实现求值器
5. 如何添加新功能

现在你可以基于这个模板创建自己的编程语言了！

## 下一步

- 添加类型系统
- 添加模块系统
- 添加编译器后端
- 添加包管理器
- 添加 LSP 支持

## 参考

- [OCaml 编程语言实现最佳实践](BEST_PRACTICES_GUIDE.md)
- [MyLang 架构文档](ARCHITECTURE.md)
- [MyLang 开发指南](CONTRIBUTING.md)
