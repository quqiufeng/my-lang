# MyLang

A simple functional programming language implemented in OCaml.

## Features

- **Integer arithmetic**: `+`, `-`, `*`, `/`
- **Boolean logic**: `&&`, `||`, `not`
- **Comparison operators**: `=`, `<>`, `<`, `<=`, `>`, `>=`
- **Variable binding**: `let x = expr in expr`
- **First-class functions**: `fun x -> expr`
- **Function application**: `f arg`
- **Conditionals**: `if expr then expr else expr`

## Example Programs

```ocaml
(* Arithmetic *)
1 + 2 * 3        (* => 7 *)

(* Variable binding *)
let x = 10 in x + 5   (* => 15 *)

(* Functions *)
let add = fun x -> fun y -> x + y in add 3 4   (* => 7 *)

(* Conditionals *)
if 1 < 2 then 100 else 200   (* => 100 *)

(* Boolean logic *)
true && false || true   (* => true *)
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
```

## Architecture

```
Source Code → Lexer → Parser → AST → Evaluator → Value
     (string)  (tokens)  (tree)  (expr)   (result)
```

## Implementation Notes

- **Lexer**: Uses `ocamllex` to tokenize source code
- **Parser**: Uses `menhir` for LR(1) grammar parsing
- **Evaluator**: Tree-walking interpreter with lexical scoping
- **Error Handling**: Syntax errors, parse errors, and runtime errors are all caught and reported

## Future Improvements

- [ ] Type inference system (Hindley-Milner)
- [ ] Pattern matching
- [ ] Recursive functions (`let rec`)
- [ ] Lists and tuples
- [ ] String type
- [ ] Modules and imports
- [ ] Bytecode compiler + VM

## License

MIT
