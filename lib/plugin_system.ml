(** 后端插件系统

    提供可扩展的后端编译插件接口。
    允许注册新的编译目标而无需修改核心代码。
*)

open Core
open Ast

type backend_name = string

type compile_result = {
  success : bool;
  output : string option;  (* 生成的代码/文件路径 *)
  errors : string list;
}

type backend_plugin = {
  name : backend_name;
  description : string;
  compile : expr -> compile_result;
  compile_file : string -> string -> compile_result;  (* input_file -> output_file -> result *)
}

(** 注册表 *)
let registry : (backend_name, backend_plugin) Hashtbl.t = Hashtbl.create (module String)

(** 注册后端插件 *)
let register plugin =
  Hashtbl.set registry ~key:plugin.name ~data:plugin;
  printf "[Plugin] Registered backend: %s\n" plugin.name

(** 查找后端插件 *)
let find name =
  Hashtbl.find registry name

(** 列出所有已注册的后端 *)
let list_backends () =
  Hashtbl.to_alist registry
  |> List.map ~f:(fun (name, plugin) -> (name, plugin.description))

(** 编译到指定后端 *)
let compile_to backend_name expr =
  match find backend_name with
  | Some plugin -> plugin.compile expr
  | None -> { success = false; output = None; errors = ["Unknown backend: " ^ backend_name] }

(** 编译文件到指定后端 *)
let compile_file_to backend_name input_file output_file =
  match find backend_name with
  | Some plugin -> plugin.compile_file input_file output_file
  | None -> { success = false; output = None; errors = ["Unknown backend: " ^ backend_name] }

(** 创建标准编译结果的辅助函数 *)
let make_success ?output () = { success = true; output; errors = [] }
let make_failure errors = { success = false; output = None; errors }

(** 默认后端：解释执行 *)
let interpreter_backend = {
  name = "interpreter";
  description = "解释执行 (默认)";
  compile = (fun expr ->
    try
      let value, _env = Eval.eval [] expr in
      make_success ~output:(Ast.string_of_value value) ()
    with
    | Eval.RuntimeError (msg, _) -> make_failure ["Runtime error: " ^ msg]
    | exn -> make_failure ["Error: " ^ Exn.to_string exn]);
  compile_file = (fun input_file _output_file ->
    try
      let content = In_channel.read_all input_file in
      let lexbuf = Lexing.from_string content in
      let expr = Parser.prog Lexer.read lexbuf in
      let value, _env = Eval.eval [] expr in
      make_success ~output:(Ast.string_of_value value) ()
    with
    | Eval.RuntimeError (msg, _) -> make_failure ["Runtime error: " ^ msg]
    | exn -> make_failure ["Error: " ^ Exn.to_string exn]);
}

(** 字节码后端 *)
let bytecode_backend = {
  name = "bytecode";
  description = "编译为栈字节码并执行";
  compile = (fun expr ->
    try
      let ctx = Compiler.new_ctx () in
      Compiler.compile_expr ctx expr;
      let code = Array.of_list (List.rev ctx.Compiler.code) in
      let result = Vm.run code in
      make_success ~output:(Vm.string_of_vm_value result) ()
    with
    | Vm.VMError msg -> make_failure ["VM error: " ^ msg]
    | exn -> make_failure ["Error: " ^ Exn.to_string exn]);
  compile_file = (fun _input_file _output_file ->
    make_failure ["File compilation not yet implemented for bytecode backend"]);
}

(** 寄存器 VM 后端 *)
let reg_vm_backend = {
  name = "reg-vm";
  description = "编译为寄存器字节码并执行";
  compile = (fun expr ->
    try
      let prog = Reg_compiler.compile_program [expr] in
      let result = Reg_vm.execute prog in
      make_success ~output:(Reg_bytecode.string_of_reg_value result) ()
    with
    | Reg_vm.RegVMError msg -> make_failure ["Register VM error: " ^ msg]
    | exn -> make_failure ["Error: " ^ Exn.to_string exn]);
  compile_file = (fun _input_file _output_file ->
    make_failure ["File compilation not yet implemented for reg-vm backend"]);
}

(** WASM 文本后端 *)
let wasm_text_backend = {
  name = "wasm";
  description = "编译为 WASM 文本格式 (.wat)";
  compile = (fun expr ->
    try
      let wat = let bytecode = Compiler.compile expr in Wasm_backend.generate_wasm bytecode in
      make_success ~output:wat ()
    with exn -> make_failure ["Error: " ^ Exn.to_string exn]);
  compile_file = (fun input_file output_file ->
    try
      let content = In_channel.read_all input_file in
      let lexbuf = Lexing.from_string content in
      let expr = Parser.prog Lexer.read lexbuf in
      let wat = let bytecode = Compiler.compile expr in Wasm_backend.generate_wasm bytecode in
      Out_channel.write_all output_file ~data:wat;
      make_success ~output:output_file ()
    with exn -> make_failure ["Error: " ^ Exn.to_string exn]);
}

(** LLVM IR 后端 *)
let llvm_ir_backend = {
  name = "llvm";
  description = "编译为 LLVM IR";
  compile = (fun expr ->
    try
      let prog = Reg_compiler.compile_program [expr] in
      let ir = Llvm_backend.generate_llvm_ir prog in
      make_success ~output:ir ()
    with exn -> make_failure ["Error: " ^ Exn.to_string exn]);
  compile_file = (fun input_file output_file ->
    try
      let content = In_channel.read_all input_file in
      let lexbuf = Lexing.from_string content in
      let expr = Parser.prog Lexer.read lexbuf in
      let prog = Reg_compiler.compile_program [expr] in
      let ir = Llvm_backend.generate_llvm_ir prog in
      Out_channel.write_all output_file ~data:ir;
      make_success ~output:output_file ()
    with exn -> make_failure ["Error: " ^ Exn.to_string exn]);
}

(** 注册所有默认后端 *)
let () =
  register interpreter_backend;
  register bytecode_backend;
  register reg_vm_backend;
  register wasm_text_backend;
  register llvm_ir_backend
