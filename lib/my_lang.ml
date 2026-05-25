(** MyLang 语言库入口 *)

module Ast = Ast
module Eval = Eval
module Types = Types
module Typeinfer = Typeinfer
module Bytecode = Bytecode
module Compiler = Compiler
module Vm = Vm
module Gc = Gc
module Lexer = Lexer
module Parser = Parser
module Wasm_backend = Wasm_backend
module Package_manager = Package_manager
module Lsp_server = Lsp_server

let parse (s : string) : Ast.expr =
  let lexbuf = Lexing.from_string s in
  Parser.prog Lexer.read lexbuf

(** 预处理 import，收集导入文件中的类型绑定 *)
let rec preprocess_imports env expr =
  match expr with
  | Ast.EApp (Ast.EVar "import", Ast.EString filename) ->
      let content =
        try Core.In_channel.read_all filename
        with Sys_error msg -> raise (Types.TypeError ("Cannot import file: " ^ msg))
      in
      let lexbuf = Lexing.from_string content in
      let imported_expr = Parser.prog Lexer.read lexbuf in
      Typeinfer.extract_bindings env imported_expr
  | Ast.ESeq (e1, e2) | Ast.ELet (_, e1, e2) | Ast.ELetRec (_, e1, e2) ->
      let env' = preprocess_imports env e1 in
      preprocess_imports env' e2
  | Ast.EIf (cond, t, f) ->
      let env' = preprocess_imports env cond in
      let env'' = preprocess_imports env' t in
      preprocess_imports env'' f
  | Ast.EMatch (e, cases) ->
      let env' = preprocess_imports env e in
      List.fold_left (fun env (_, body) -> preprocess_imports env body) env' cases
  | _ -> env

let typecheck (e : Ast.expr) : Types.t =
  let env = preprocess_imports Eval.builtin_type_env e in
  Typeinfer.typecheck_with_env env e

let eval (e : Ast.expr) : Ast.value = Eval.run e

let compile (e : Ast.expr) : Bytecode.code = Compiler.compile e

let run_bytecode (code : Bytecode.code) : Vm.vm_value = Vm.run code

let compile_to_wasm (e : Ast.expr) : string =
  let bytecode = compile e in
  Wasm_backend.generate_wasm bytecode

let run (s : string) : Ast.value =
  let expr = parse s in
  let _ = typecheck expr in
  eval expr

let run_exn s =
  try Ok (run s) with
  | Lexer.SyntaxError msg -> Error ("Syntax error: " ^ msg)
  | Parser.Error -> Error "Parse error"
  | Eval.RuntimeError (msg, pos) ->
      let pos_str =
        match pos with
        | Some p -> " at " ^ Ast.string_of_pos p
        | None -> ""
      in
      Error ("Runtime error" ^ pos_str ^ ": " ^ msg)
  | Types.TypeError msg -> Error ("Type error: " ^ msg)
  | Vm.VMError msg -> Error ("VM error: " ^ msg)
