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
module Wasm_binary = Wasm_binary
module Package_manager = Package_manager
module Registry = Registry
module Lsp_server = Lsp_server
module Reg_bytecode = Reg_bytecode
module Reg_vm = Reg_vm
module Reg_compiler = Reg_compiler
module Traits = Traits
module Ffi = Ffi
module Ownership = Ownership
module Jit = Jit
module Generational_gc = Generational_gc
module Gc_bridge = Gc_bridge
module Actor = Actor
module Module_dependency = Module_dependency
module Compilation_cache = Compilation_cache
module Incremental_compile = Incremental_compile
module Debugger = Debugger
module Llvm_backend = Llvm_backend
module Error_context = Error_context
module Diagnostics = Diagnostics
module Optimizer = Optimizer
module Llvm_compile = Llvm_compile

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

let run ?(check_ownership=true) (s : string) : Ast.value =
  let expr = parse s in
  let _ = typecheck expr in
  if check_ownership then Ownership.check_program [expr];
  eval expr

(** 解析并返回 lexbuf（用于错误位置报告） *)
let parse_with_lexbuf (s : string) : Lexing.lexbuf * Ast.expr =
  let lexbuf = Lexing.from_string s in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "<input>" };
  let expr = Parser.prog Lexer.read lexbuf in
  (lexbuf, expr)

(** 使用 error_context 的增强版错误处理 *)
let run_exn ?(check_ownership=true) ?(source="") s =
  let lexbuf_opt = ref None in
  let diag = Diagnostics.create () in
  try
    let lexbuf = Lexing.from_string s in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = (if source = "" then "<input>" else source) };
    lexbuf_opt := Some lexbuf;
    Parser_recovery.set_reporter (fun msg pos ->
      let open Lexing in
      Diagnostics.add_error diag
        ~phase:Diagnostics.Parsing
        ~line:pos.pos_lnum
        ~col:(max 1 (pos.pos_cnum - pos.pos_bol))
        msg
    );
    let expr = Parser.prog Lexer.read lexbuf in
    let _ = typecheck expr in
    if check_ownership then Ownership.check_program [expr];
    if Diagnostics.has_errors diag then
      Error (Diagnostics.format_all diag)
    else
      Ok (eval expr)
  with
  | exn ->
      let ctx = Error_context.create () in
      let d = Error_context.from_exception exn !lexbuf_opt in
      Error_context.add_diagnostic ctx ~severity:Error ~message:d.message
        ?file:(if d.file = "" then None else Some d.file)
        ~line:d.line ~col:d.col ?source_line:d.source_line
        ~highlight_len:d.highlight_len ?help:d.help ();
      Error (Error_context.format_all ctx)

(** 将 Ast.value 转换为 GC heap_obj *)
let rec value_to_gc_obj heap = function
  | Ast.VInt n -> Generational_gc.make_int heap n
  | Ast.VBool b -> Generational_gc.make_bool heap b
  | Ast.VString s -> Generational_gc.make_string heap s
  | Ast.VUnit -> Generational_gc.make_int heap 0
  | Ast.VList vs -> Generational_gc.make_list heap (List.map (value_to_gc_obj heap) vs)
  | Ast.VTuple vs -> Generational_gc.make_tuple heap (List.map (value_to_gc_obj heap) vs)
  | Ast.VRef r -> Generational_gc.make_ref heap (value_to_gc_obj heap !r)
  | Ast.VArray arr -> Generational_gc.make_array heap (List.map (value_to_gc_obj heap) (Array.to_list arr))
  | Ast.VRecord fields ->
      Generational_gc.allocate heap (Generational_gc.ORecord (List.map (fun (k, r) -> (k, value_to_gc_obj heap !r)) fields))
  | Ast.VFun _ -> Generational_gc.make_int heap 0  (* 简化为整数 *)
  | _ -> Generational_gc.make_int heap 0

(** 将 Vm.vm_value 转换为 GC heap_obj *)
let rec vm_value_to_gc_obj heap = function
  | Vm.VInt n -> Generational_gc.make_int heap n
  | Vm.VBool b -> Generational_gc.make_bool heap b
  | Vm.VString s -> Generational_gc.make_string heap s
  | Vm.VUnit -> Generational_gc.make_int heap 0
  | Vm.VNil -> Generational_gc.make_list heap []
  | Vm.VList vs -> Generational_gc.make_list heap (List.map (vm_value_to_gc_obj heap) vs)
  | Vm.VTuple vs -> Generational_gc.make_tuple heap (List.map (vm_value_to_gc_obj heap) vs)
  | Vm.VRef r -> Generational_gc.make_ref heap (vm_value_to_gc_obj heap !r)
  | Vm.VArray arr -> Generational_gc.make_array heap (List.map (vm_value_to_gc_obj heap) (Array.to_list arr))
  | Vm.VRecord fields ->
      Generational_gc.allocate heap (Generational_gc.ORecord (List.map (fun (k, r) -> (k, vm_value_to_gc_obj heap !r)) fields))
  | _ -> Generational_gc.make_int heap 0

(** 将 Reg_bytecode.reg_value 转换为 GC heap_obj *)
let rec reg_value_to_gc_obj heap = function
  | Reg_bytecode.RVInt n -> Generational_gc.make_int heap n
  | Reg_bytecode.RVBool b -> Generational_gc.make_bool heap b
  | Reg_bytecode.RVString s -> Generational_gc.make_string heap s
  | Reg_bytecode.RVUnit -> Generational_gc.make_int heap 0
  | Reg_bytecode.RVNil -> Generational_gc.make_list heap []
  | Reg_bytecode.RVList vs -> Generational_gc.make_list heap (List.map (reg_value_to_gc_obj heap) vs)
  | Reg_bytecode.RVTuple vs -> Generational_gc.make_tuple heap (List.map (reg_value_to_gc_obj heap) vs)
  | Reg_bytecode.RVRef r -> Generational_gc.make_ref heap (reg_value_to_gc_obj heap !r)
  | _ -> Generational_gc.make_int heap 0

(** 带 GC 的执行：执行代码后将结果注册为 GC 根并触发 GC *)
let run_with_gc ?(check_ownership=true) (s : string) : Ast.value * string =
  let heap = Gc_bridge.get_heap () in
  let result = run ~check_ownership s in
  let root = value_to_gc_obj heap result in
  Generational_gc.force_gc heap [root];
  let stats = Generational_gc.heap_stats heap in
  (result, stats)

(** 带 GC 的栈 VM 执行 *)
let run_bytecode_with_gc (code : Bytecode.code) : Vm.vm_value * string =
  let heap = Gc_bridge.get_heap () in
  let result = Vm.run code in
  let root = vm_value_to_gc_obj heap result in
  Generational_gc.force_gc heap [root];
  let stats = Generational_gc.heap_stats heap in
  (result, stats)

(** 带 GC 的寄存器 VM 执行 *)
let run_reg_vm_with_gc prog : Reg_bytecode.reg_value * string =
  let heap = Gc_bridge.get_heap () in
  let result = Reg_vm.execute prog in
  let root = reg_value_to_gc_obj heap result in
  Generational_gc.force_gc heap [root];
  let stats = Generational_gc.heap_stats heap in
  (result, stats)
