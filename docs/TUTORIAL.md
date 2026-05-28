# 从零开始创建编程语言

本教程指导你使用 MyLang 框架创建自己的编程语言。

---

## 快速开始

```bash
# 复制模板
cp -r templates/basic_language my_language
cd my_language

# 构建并运行
dune build
dune exec bin/main.exe
```

---

## 核心步骤

### 1. 定义 AST

```ocaml
(* ast.ml *)
type expr =
  | EInt of int
  | EVar of string
  | ELet of string * expr * expr
  | EFun of string list * expr
  | EApp of expr * expr list
  | EBinary of op * expr * expr
```

### 2. 实现词法分析器

```ocaml
(* lexer.mll *)
rule read = parse
  | ['0'-'9']+ { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | "let" { LET }
  | ['a'-'z']+ { IDENT (Lexing.lexeme lexbuf) }
  | '+' { PLUS }
  | eof { EOF }
```

### 3. 实现语法分析器

```ocaml
(* parser.mly *)
%token <int> INT
%token <string> IDENT
%token LET IN PLUS EOF

%start <Ast.expr> prog

%%
prog: e = expr; EOF { e }
expr: 
  | LET; x = IDENT; EQ; e1 = expr; IN; e2 = expr { ELet (x, e1, e2) }
  | e1 = expr; PLUS; e2 = expr { EBinary (Add, e1, e2) }
  | n = INT { EInt n }
  | x = IDENT { EVar x }
```

### 4. 实现求值器

```ocaml
(* eval.ml *)
let rec eval env = function
  | EInt n -> VInt n
  | EVar x -> List.assoc x env
  | ELet (x, e1, e2) ->
      let v1 = eval env e1 in
      eval ((x, v1) :: env) e2
  | EBinary (Add, e1, e2) ->
      let VInt a = eval env e1 in
      let VInt b = eval env e2 in
      VInt (a + b)
```

---

## 添加新功能

### 添加字符串

```ocaml
(* ast.ml *)
| EString of string

(* lexer.mll *)
| '"' { string (Buffer.create 16) lexbuf }

(* eval.ml *)
| EString s -> VString s
```

### 添加函数

```ocaml
(* ast.ml *)
| VFun of string list * expr * env

(* eval.ml *)
| EFun (params, body) -> VFun (params, body, env)
| EApp (f, args) ->
    let VFun (params, body, env') = eval env f in
    let argvs = List.map (eval env) args in
    eval (List.combine params argvs @ env') body
```

### 添加模式匹配

```ocaml
(* ast.ml *)
| EMatch of expr * (pattern * expr) list

(* eval.ml *)
| EMatch (e, cases) ->
    let v = eval env e in
    List.find_map (fun (p, body) ->
      match match_pattern p v with
      | Some bindings -> Some (eval (bindings @ env) body)
      | None -> None
    ) cases
```

---

## 添加类型系统

```ocaml
(* typeinfer.ml *)
let rec infer env = function
  | EInt _ -> TInt
  | EBool _ -> TBool
  | EVar x -> List.assoc x env
  | EBinary (Add, e1, e2) ->
      unify (infer env e1) TInt;
      unify (infer env e2) TInt;
      TInt
```

---

## 测试

```bash
# 运行测试
dune test

# 运行示例
echo "let x = 42 in x + 1" > test.ml
dune exec bin/main.exe test.ml
```

---

## 参考

- [最佳实践](BEST_PRACTICES.md)
- [架构设计](ARCHITECTURE.md)
- [标准库](STDLIB.md)
