# MyLang

A simple functional programming language implemented in OCaml.

## Features

- **Integer arithmetic**: `+`, `-`, `*`, `/`
- **Boolean logic**: `&&`, `||`, `not`
- **Comparison operators**: `=`, `<>`, `<`, `<=`, `>`, `>=`
- **Variable binding**: `let x = expr in expr`
- **Recursive binding**: `let rec f = fun x -> ... in ...`
- **First-class functions**: `fun x -> expr`
- **Function application**: `f arg`
- **Conditionals**: `if expr then expr else expr`
- **Strings**: `"hello world"`
- **Lists**: `[1, 2, 3]`, `1 :: [2, 3]`
- **Tuples**: `(1, true, "hello")`
- **Sequence**: `expr1; expr2`
- **Pattern Matching**: `match expr with | pattern -> expr | ...`

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
```

## Build

```bash
eval $(opam env --switch=default)
dune build
```

## Run REPL

```bash
dune exec my_lang
```

## Run a File

```bash
dune exec my_lang -- examples/test.ml
```

## Test

```bash
dune test
```

## Project Structure

```
lib/
  ast.ml       - Abstract Syntax Tree definition
  lexer.mll    - Lexical analyzer (ocamllex)
  parser.mly   - Syntax analyzer (menhir)
  eval.ml      - Expression evaluator
  my_lang.ml   - Library entry point
bin/
  main.ml      - CLI / REPL
test/
  test_my_lang.ml - Test suite
```

## Architecture

### 解释器模式（默认）
```
Source Code → Lexer → Parser → AST → Type Checker → Evaluator → Value
     (string)  (tokens)  (tree)  (expr)   (types)      (result)
```

### 字节码模式
```
Source Code → Lexer → Parser → AST → Compiler → Bytecode → VM → Value
     (string)  (tokens)  (tree)  (expr)   (code)     (instr)   (result)
```

## Implementation Notes

- **Lexer**: Uses `ocamllex` to tokenize source code
- **Parser**: Uses `menhir` for LR(1) grammar parsing
- **Type Checker**: Hindley-Milner type inference with let-polymorphism
- **Evaluator**: Tree-walking interpreter with lexical scoping and closures
- **Closures**: Functions capture their defining environment
- **Recursion**: `let rec` creates self-referential closures
- **Compiler**: AST-to-bytecode compiler with label backpatching
- **VM**: Stack-based virtual machine with frame-based function calls
- **Performance**: 
  - Type inference uses `Int.Map` for substitutions (O(log n) lookup)
  - Bytecode compiler accumulates instructions in a list (O(1) emit)
  - VM uses mutable state with minimal allocation
- **Error Handling**: Syntax errors, parse errors, type errors, and runtime errors are all caught and reported with source positions

## Future Improvements

- [x] Strings
- [x] Lists and cons operator
- [x] Tuples
- [x] `let rec` recursive functions
- [x] Sequence expression (`e1; e2`)
- [x] Pattern matching
- [x] String concatenation operator
- [x] List head/tail builtins
- [x] Type inference system (Hindley-Milner)
- [x] Modules and imports (`import "file.ml"`)
- [x] Bytecode compiler + VM
- [ ] Modules and imports
- [ ] Bytecode compiler + VM

## License

MIT
