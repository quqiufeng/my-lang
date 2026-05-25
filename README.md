# MyLang

A simple functional programming language implemented in OCaml.

## How to Implement a Programming Language (in OCaml)

Implementing a language is simpler than it sounds. You break it into **four stages**:

```
Source Code → Lexer → Parser → AST → Evaluator → Result
   "1+2"      tokens   tree    value
```

### 1. Lexer (词法分析)

Turns raw text into a stream of **tokens** (the smallest meaningful units).

```ocaml
(* lib/lexer.mll *)
rule read = parse
  | digit+ as n   { INT (int_of_string n) }   (* "42" → INT 42 *)
  | "+"           { PLUS }
  | "let"         { LET }
  | ident as s    { IDENT s }                 (* "x" → IDENT "x" *)
  | whitespace    { read lexbuf }             (* skip spaces *)
  | eof           { EOF }
```

Input: `"let x = 1 + 2"`  
Output: `[LET; IDENT "x"; EQ; INT 1; PLUS; INT 2; EOF]`

### 2. Parser (语法分析)

Turns tokens into an **Abstract Syntax Tree (AST)** — a tree that represents the *structure* of the program, ignoring syntax noise like parentheses and keywords.

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

Input: `[LET; IDENT "x"; EQ; INT 1; PLUS; INT 2; EOF]`  
Output: `ELet ("x", EAdd (EInt 1, EInt 2), EVar "x")`

### 3. AST (抽象语法树)

The heart of your language. You define what a program *is* using OCaml's algebraic data types.

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

### 4. Evaluator (求值器)

Recursively walks the AST and computes the result. An **environment** (a list of variable bindings) tracks what each name means.

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

The key idea: **functions capture their environment** (this is a closure). When you call `f 5`, `f` runs with the variables it could see when it was *defined*, not where it is *called*.

---

## Features

- **Integer arithmetic**: `+`, `-`, `*`, `/`
- **Boolean logic**: `&&`, `||`, `not`
- **Comparison**: `=`, `<>`, `<`, `<=`, `>`, `>=`
- **Variable binding**: `let x = expr in expr`
- **Recursive binding**: `let rec f = fun x -> ... in ...`
- **First-class functions**: `fun x -> expr`
- **Conditionals**: `if expr then expr else expr`
- **Strings**: `"hello world"`, concatenation `^`
- **Lists**: `[1, 2, 3]`, `1 :: [2, 3]`
- **Tuples**: `(1, true, "hello")`
- **Sequence**: `expr1; expr2`
- **Pattern matching**: `match expr with | pattern -> expr | ...`
- **Static type inference**: Hindley-Milner with let-polymorphism
- **Bytecode compiler + VM**: compile to bytecode for faster execution
- **Module imports**: `import "file.ml"`

---

## How to Add a New Syntax Feature

Let's add a **`>` (greater-than)** operator as an example. You need to touch **4 files**:

### Step 1: AST — Add the new expression node

```ocaml
(* lib/ast.ml *)
type expr =
  | ...
  | EGt of expr * expr    (* NEW: e1 > e2 *)
```

### Step 2: Lexer — Add the new token

```ocaml
(* lib/lexer.mll *)
rule read = parse
  | ...
  | ">"           { GT }    (* NEW *)
```

### Step 3: Parser — Add the grammar rule

```ocaml
(* lib/parser.mly *)
%token GT                    (* NEW *)
%nonassoc EQ NEQ LT LE GT GE

expr:
  | ...
  | e1 = expr GT e2 = expr  { EGt (e1, e2) }   (* NEW *)
```

### Step 4: Evaluator — Define what it does

```ocaml
(* lib/eval.ml *)
let rec eval env expr =
  match expr with
  | ...
  | EGt (e1, e2) ->
      (match eval env e1, eval env e2 with
       | VInt a, VInt b -> VBool (a > b))
```

That's it. Rebuild with `dune build` and `3 > 2` now evaluates to `true`.

### Adding a Control Flow Feature (e.g., `while`)

For features that need **bytecode support** (like loops), you also need:

1. **Bytecode instruction**: Add `Jump` / `JumpIfFalse` to `lib/bytecode.ml`
2. **Compiler**: Emit jumps in `lib/compiler.ml`
3. **VM**: Execute jumps in `lib/vm.ml`

See `claude.md` for detailed compiler/VM development practices.

---

## Example Programs

```ocaml
(* Arithmetic *)
1 + 2 * 3        (* => 7 *)

(* Variable binding *)
let x = 10 in x + 5   (* => 15 *)

(* Functions *)
let add = fun x -> fun y -> x + y in add 3 4   (* => 7 *)

(* Recursion *)
let rec factorial = fun n ->
  if n = 0 then 1 else n * factorial (n - 1)
in factorial 5   (* => 120 *)

(* Strings *)
let greeting = "Hello" in greeting ^ " World"   (* => "Hello World" *)

(* Lists *)
let xs = [1, 2, 3] in 1 :: xs   (* => [1, 1, 2, 3] *)

(* Tuples *)
let pair = (1, "hello") in pair   (* => (1, "hello") *)

(* Pattern Matching *)
match [1, 2, 3] with
| [] -> 0
| h :: t -> h + length t   (* => 3 *)

(* Sequence *)
let x = 1 in
let y = 2 in
x + y; x * y   (* => 2 *)

(* Type inference - polymorphism *)
let id = fun x -> x in
(id 5, id true)   (* => (5, true) *)
```

---

## Quick Start

### Build

```bash
eval $(opam env --switch=default)
dune build
```

### Run REPL

```bash
dune exec my_lang
```

### Run a File

```bash
dune exec my_lang -- examples/test.ml
```

### Test

```bash
dune test
```

---

## Project Structure

```
lib/
  ast.ml         - Abstract Syntax Tree definition
  lexer.mll      - Lexical analyzer (ocamllex)
  parser.mly     - Syntax analyzer (menhir)
  eval.ml        - Tree-walking interpreter
  types.ml       - Type system and unification
  typeinfer.ml   - Hindley-Milner type inference
  bytecode.ml    - Bytecode instruction definitions
  compiler.ml    - AST-to-bytecode compiler
  vm.ml          - Stack-based virtual machine
  my_lang.ml     - Library entry point
bin/
  main.ml        - CLI / REPL
test/
  test_my_lang.ml   - Interpreter tests
  test_bytecode.ml  - Bytecode VM tests
design.md        - Detailed design document
claude.md        - Development best practices
```

---

## Architecture

### Interpreter Mode (Default)
```
Source Code → Lexer → Parser → AST → Type Checker → Evaluator → Value
```

### Bytecode Mode
```
Source Code → Lexer → Parser → AST → Compiler → Bytecode → VM → Value
```

**Key Design Decisions:**
- **Lexical scoping**: Functions capture their defining environment (closures)
- **Strict evaluation**: Arguments are evaluated before function calls
- **Let-polymorphism**: `let id = fun x -> x` gets type `'a -> 'a`
- **Two execution modes**: Interpreter for simplicity, bytecode VM for performance

---

## Implementation Notes

- **Lexer**: Uses `ocamllex` to tokenize source code; tracks line/column positions for error reporting
- **Parser**: Uses `menhir` for LR(1) grammar parsing with shift/reduce conflict resolution
- **Type Checker**: Hindley-Milner type inference with `Int.Map` for O(log n) substitutions
- **Evaluator**: Tree-walking interpreter with environment chaining
- **Compiler**: Accumulates instructions in a list (O(1) emit), uses backpatching for control flow
- **VM**: Stack-based with mutable refs for minimal allocation; handles recursion via `ReturnExn` exception
- **Error Handling**: All errors (syntax, parse, type, runtime, VM) are caught and reported with source positions

---

## Roadmap

- [x] Core language (int, bool, let, fun, if)
- [x] Strings and concatenation
- [x] Lists and cons operator
- [x] Tuples
- [x] `let rec` recursive functions
- [x] Sequence expression (`e1; e2`)
- [x] Pattern matching
- [x] Hindley-Milner type inference
- [x] Module imports (`import "file.ml"`)
- [x] Bytecode compiler + VM
- [ ] Tail call optimization (TCO)
- [ ] Algebraic data types (ADT)
- [ ] Type annotations (`: int`, `: bool`)
- [ ] Garbage collection

---

## License

MIT
