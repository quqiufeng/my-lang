(** MyLang - 交互式解释器与编译器 *)

open Core
open My_lang

let print_usage () =
  print_endline "MyLang - 简单的函数式编程语言";
  print_endline "";
  print_endline "用法:";
  print_endline "  my_lang                    启动 REPL";
  print_endline "  my_lang <file>             运行文件";
  print_endline "  my_lang compile <file>     编译为字节码";
  print_endline "  my_lang compile --wasm <file>     编译为 WASM 文本 (.wat)";
  print_endline "  my_lang compile --wasm-bin <file> 编译为 WASM 二进制 (.wasm)";
  print_endline "  my_lang compile --reg-vm <file>   编译为寄存器字节码并执行";
  print_endline "  my_lang compile --jit <file>      JIT 编译并执行";
  print_endline "  my_lang compile --llvm <file>     生成 LLVM IR";
  print_endline "  my_lang compile --scheme <file>   编译为 Scheme 代码";
  print_endline "  my_lang compile --aot <file>      AoT 编译为独立可执行文件";
  print_endline "  my_lang debug <file>              交互式调试";
  print_endline "";
  print_endline "包管理:";
  print_endline "  my_lang init <name>        初始化新项目";
  print_endline "  my_lang build              增量构建项目";
  print_endline "  my_lang build --parallel   并行增量构建";
  print_endline "  my_lang build --no-cache   清除缓存并构建";
  print_endline "  my_lang deps <file>        显示文件依赖图";
  print_endline "  my_lang install            安装依赖";
  print_endline "  my_lang test               运行测试";
  print_endline "  my_lang info               显示项目信息";
  print_endline "";
  print_endline "包注册表:";
  print_endline "  my_lang search <query>     搜索包";
  print_endline "  my_lang add <name>[@ver]   添加依赖";
  print_endline "  my_lang list               列出已安装包";
  print_endline "  my_lang publish            发布当前包";
  print_endline "";
  print_endline "LSP 服务器:";
  print_endline "  my_lang lsp                启动 LSP 语言服务器";
  print_endline "";
  print_endline "编译选项:";
  print_endline "  --wasm                     输出 WASM 文本格式 (.wat)";
  print_endline "  --output <file>            指定输出文件";
  print_endline "";
  print_endline "REPL 命令:";
  print_endline "  :help             显示帮助";
  print_endline "  :quit / :q        退出";
  print_endline "  :type <expr>      显示表达式类型";
  print_endline "";
  print_endline "示例:";
  print_endline "  1 + 2 * 3";
  print_endline "  let x = 10 in x + 5";
  print_endline "  fun x -> x + 1";
  print_endline "  let rec factorial = fun n -> if n = 0 then 1 else n * factorial (n - 1)";
  print_endline "  match [1, 2, 3] with | [] -> 0 | h :: t -> h + length t"

let show_type line =
  try
    let expr = My_lang.parse line in
    let t = My_lang.typecheck expr in
    Printf.printf "- : %s\n%!" (My_lang.Types.string_of_type t)
  with
  | exn -> Printf.printf "Error: %s\n%!" (Exn.to_string exn)

let eval_line line =
  match My_lang.run_exn line with
  | Ok v -> print_endline (My_lang.Ast.string_of_value v)
  | Error msg -> print_endline msg

let rec repl () =
  print_string "my-lang> ";
  Out_channel.flush stdout;
  match In_channel.input_line In_channel.stdin with
  | None -> print_endline "\nBye!"
  | Some "" -> repl ()
  | Some line when String.is_prefix line ~prefix:":help" ->
      print_usage ();
      repl ()
  | Some ":quit" | Some ":q" | Some "exit" | Some "quit" ->
      print_endline "Bye!"
  | Some line when String.is_prefix line ~prefix:":type " ->
      let code = String.chop_prefix_exn line ~prefix:":type " in
      show_type code;
      repl ()
  | Some line ->
      eval_line line;
      repl ()

let run_file filename =
  let content = In_channel.read_all filename in
  match My_lang.run_exn content with
  | Ok v -> print_endline (My_lang.Ast.string_of_value v)
  | Error msg ->
      print_endline msg;
      exit 1

let debug_file filename =
  try
    let content = In_channel.read_all filename in
    let expr = My_lang.parse content in
    let _ = My_lang.typecheck expr in
    let prog = My_lang.Reg_compiler.compile_program [expr] in
    let state = My_lang.Debugger.init_debug_state prog in
    
    Printf.printf "=== Debugger ===\n";
    Printf.printf "File: %s\n" filename;
    Printf.printf "Commands: run(r), continue(c), step(s), next(n), finish(f), break <func> <pc>, info(i), regs, vars, stack, quit(q)\n";
    
    let rec debug_loop () =
      print_string "(debug) ";
      Out_channel.flush stdout;
      match In_channel.input_line In_channel.stdin with
      | None -> ()
      | Some "run" | Some "r" ->
          let result = My_lang.Debugger.run state in
          Printf.printf "Result: %s\n" (My_lang.Reg_bytecode.string_of_reg_value result);
          if state.paused then (
            Printf.printf "Paused at %s\n" (My_lang.Debugger.string_of_location state);
            debug_loop ()
          ) else print_endline "Execution finished."
      | Some "continue" | Some "c" ->
          let result = My_lang.Debugger.continue state in
          Printf.printf "Result: %s\n" (My_lang.Reg_bytecode.string_of_reg_value result);
          if state.paused then (
            Printf.printf "Paused at %s\n" (My_lang.Debugger.string_of_location state);
            debug_loop ()
          ) else print_endline "Execution finished."
      | Some "step" | Some "s" ->
          let cont = My_lang.Debugger.step_into state in
          if cont then (
            Printf.printf "%s\n" (My_lang.Debugger.string_of_location state);
            Printf.printf "%s\n" (My_lang.Debugger.disassemble_current state ~window:2);
            debug_loop ()
          ) else print_endline "Execution finished."
      | Some "next" | Some "n" ->
          let cont = My_lang.Debugger.step_over state in
          if cont then (
            Printf.printf "%s\n" (My_lang.Debugger.string_of_location state);
            Printf.printf "%s\n" (My_lang.Debugger.disassemble_current state ~window:2);
            debug_loop ()
          ) else print_endline "Execution finished."
      | Some "finish" | Some "f" ->
          let cont = My_lang.Debugger.step_out state in
          if cont then (
            Printf.printf "%s\n" (My_lang.Debugger.string_of_location state);
            Printf.printf "%s\n" (My_lang.Debugger.disassemble_current state ~window:2);
            debug_loop ()
          ) else print_endline "Execution finished."
      | Some cmd when String.is_prefix cmd ~prefix:"break " ->
          (match String.split (String.chop_prefix_exn cmd ~prefix:"break ") ~on:' ' with
           | [func; pc] ->
               let func_idx = int_of_string func in
               let pc_val = int_of_string pc in
               My_lang.Debugger.set_breakpoint state func_idx pc_val;
               Printf.printf "Breakpoint set at func=%d, pc=%d\n" func_idx pc_val;
           | _ -> print_endline "Usage: break <func_idx> <pc>");
          debug_loop ()
      | Some "info" | Some "i" ->
          Printf.printf "%s\n" (My_lang.Debugger.disassemble_current state ~window:3);
          debug_loop ()
      | Some "regs" ->
          let regs = My_lang.Debugger.get_registers state in
          List.iteri regs ~f:(fun i v ->
              Printf.printf "r%d = %s\n" i (My_lang.Reg_bytecode.string_of_reg_value v));
          debug_loop ()
      | Some "vars" ->
          let vars = My_lang.Debugger.get_variables state in
          List.iter vars ~f:(fun (name, v) ->
              Printf.printf "%s = %s\n" name (My_lang.Reg_bytecode.string_of_reg_value v));
          debug_loop ()
      | Some "stack" ->
          let trace = My_lang.Debugger.get_stack_trace state in
          List.iter trace ~f:(fun (name, pc) ->
              Printf.printf "  %s (pc=%d)\n" name pc);
          debug_loop ()
      | Some "quit" | Some "q" -> print_endline "Debugger exited."
      | Some "" -> debug_loop ()
      | Some cmd -> Printf.printf "Unknown command: %s\n" cmd; debug_loop ()
    in
    debug_loop ()
  with
  | exn ->
      Printf.printf "调试错误: %s\n" (Exn.to_string exn);
      exit 1

let compile_file ~wasm ~wasm_binary ~reg_vm ~jit ~llvm ~scheme ~aot ~output filename =
  try
    let content = In_channel.read_all filename in
    let expr = My_lang.parse content in
    let _ = My_lang.typecheck expr in
    if wasm then
      let wasm_code = My_lang.compile_to_wasm expr in
      match output with
      | Some out ->
          Out_channel.write_all out ~data:wasm_code;
          Printf.printf "WASM (text) 已写入: %s\n" out
      | None ->
          print_endline wasm_code
    else if wasm_binary then
      let wat_code = My_lang.compile_to_wasm expr in
      let wasm_binary = My_lang.Wasm_binary.compile_to_wasm_binary wat_code in
      match output with
      | Some out ->
          Out_channel.write_all out ~data:wasm_binary;
          Printf.printf "WASM (binary) 已写入: %s (%d bytes)\n" out (String.length wasm_binary)
      | None ->
          Printf.printf "WASM binary (%d bytes)\n" (String.length wasm_binary)
    else if reg_vm then
      let prog = My_lang.Reg_compiler.compile_program [expr] in
      let _ = My_lang.Reg_vm.execute prog in
      Printf.printf "寄存器 VM 执行完成\n"
    else if jit then
      let prog = My_lang.Reg_compiler.compile_program [expr] in
      let _ = My_lang.Jit.execute_jit prog in
      Printf.printf "JIT 执行完成\n"
    else if llvm then
      let prog = My_lang.Reg_compiler.compile_program [expr] in
      let llvm_ir = My_lang.Llvm_backend.generate_llvm_ir prog in
      match output with
      | Some out ->
          Out_channel.write_all out ~data:llvm_ir;
          Printf.printf "LLVM IR 已写入: %s\n" out
      | None ->
          print_endline "=== LLVM IR ===";
          print_endline llvm_ir
    else if scheme then
      let scheme_code = My_lang.Scheme_backend.compile_program expr in
      match output with
      | Some out ->
          Out_channel.write_all out ~data:scheme_code;
          Printf.printf "Scheme 代码已写入: %s\n" out
      | None ->
          print_endline "=== Scheme Code ===";
          print_endline scheme_code
    else if aot then
      let output_file = match output with
        | Some out -> out
        | None -> 
          (* 移除文件扩展名 *)
          let base = Filename.basename filename in
          let dir = Filename.dirname filename in
          let name = 
            try Filename.chop_extension base 
            with Invalid_argument _ -> base
          in
          Filename.concat dir name
      in
      match My_lang.Aot.compile_standalone filename output_file with
      | Ok msg -> Printf.printf "%s\n" msg
      | Error err -> Printf.eprintf "AoT 编译失败: %s\n" err; exit 1
    else
      let bytecode = My_lang.compile expr in
      let buf = Buffer.create 256 in
      Array.iteri bytecode ~f:(fun i instr ->
          Buffer.add_string buf (Printf.sprintf "%04d: %s\n" i (My_lang.Bytecode.string_of_instr instr)));
      let code_str = Buffer.contents buf in
      match output with
      | Some out ->
          Out_channel.write_all out ~data:code_str;
          Printf.printf "字节码已写入: %s\n" out
      | None ->
          print_endline "=== Bytecode ===";
          print_endline code_str
  with
  | exn ->
      Printf.printf "编译错误: %s\n" (Exn.to_string exn);
      exit 1

let incremental_build_project ~parallel =
  let config = Package_manager.read_config () in
  Printf.printf "Building project '%s' v%s (incremental)...\n" config.name config.version;
  
  let entry = Option.value config.entry_point ~default:"main.ml" in
  if not (Stdlib.Sys.file_exists entry) then begin
    Printf.eprintf "Entry point '%s' not found\n" entry;
    false
  end else
    let compile_fn = if parallel then
      My_lang.Incremental_compile.compile_and_link_parallel ~cache:true
    else
      My_lang.Incremental_compile.compile_and_link ~cache:true
    in
    match compile_fn entry with
    | Ok bytecode ->
        let build_dir = "build" in
        if not (Stdlib.Sys.file_exists build_dir) then Stdlib.Sys.mkdir build_dir 0o755;
        
        (* 输出字节码 *)
        let buf = Buffer.create 256 in
        Array.iteri bytecode ~f:(fun i instr ->
            Buffer.add_string buf (Printf.sprintf "%04d: %s\n" i (My_lang.Bytecode.string_of_instr instr)));
        let bc_file = build_dir ^ "/" ^ config.name ^ ".bc" in
        Out_channel.write_all bc_file ~data:(Buffer.contents buf);
        
        Printf.printf "Build successful!\n";
        Printf.printf "  Bytecode: %s\n" bc_file;
        Printf.printf "  Instructions: %d\n" (Array.length bytecode);
        true
    | Error msg ->
        Printf.eprintf "Build failed: %s\n" msg;
        false

let build_project () =
  let config = Package_manager.read_config () in
  Printf.printf "Building project '%s' v%s...\n" config.name config.version;
  
  (* 编译入口文件 *)
  let entry = Option.value config.entry_point ~default:"main.ml" in
  match
    try Some (In_channel.read_all entry)
    with Sys_error _ -> None
  with
  | Some content ->
    (try
      let expr = My_lang.parse content in
      let _ = My_lang.typecheck expr in
      
      (* 生成 WASM *)
      let wasm = My_lang.compile_to_wasm expr in
      let wasm_file = "build/" ^ config.name ^ ".wat" in
      Out_channel.write_all wasm_file ~data:wasm;
      
      Printf.printf "Build successful!\n";
      Printf.printf "  WASM: %s\n" wasm_file;
      true
    with
    | e ->
        Printf.eprintf "Build failed: %s\n" (Exn.to_string e);
        false)
  | None ->
    Printf.eprintf "Entry point '%s' not found\n" entry;
    false

let () =
  let args = Array.to_list (Sys.get_argv ()) in
  match args with
  | [_] ->
      print_usage ();
      print_endline "";
      repl ()
  | [_; "compile"; filename] -> compile_file ~wasm:false ~wasm_binary:false ~reg_vm:false ~jit:false ~llvm:false ~scheme:false ~aot:false ~output:None filename
  | [_; "compile"; "--wasm"; filename] -> compile_file ~wasm:true ~wasm_binary:false ~reg_vm:false ~jit:false ~llvm:false ~scheme:false ~aot:false ~output:None filename
  | [_; "compile"; "--wasm-bin"; filename] -> compile_file ~wasm:false ~wasm_binary:true ~reg_vm:false ~jit:false ~llvm:false ~scheme:false ~aot:false ~output:None filename
  | [_; "compile"; "--reg-vm"; filename] -> compile_file ~wasm:false ~wasm_binary:false ~reg_vm:true ~jit:false ~llvm:false ~scheme:false ~aot:false ~output:None filename
  | [_; "compile"; "--jit"; filename] -> compile_file ~wasm:false ~wasm_binary:false ~reg_vm:false ~jit:true ~llvm:false ~scheme:false ~aot:false ~output:None filename
  | [_; "compile"; "--llvm"; filename] -> compile_file ~wasm:false ~wasm_binary:false ~reg_vm:false ~jit:false ~llvm:true ~scheme:false ~aot:false ~output:None filename
  | [_; "compile"; "--scheme"; filename] -> compile_file ~wasm:false ~wasm_binary:false ~reg_vm:false ~jit:false ~llvm:false ~scheme:true ~aot:false ~output:None filename
  | [_; "compile"; "--output"; out; filename] -> compile_file ~wasm:false ~wasm_binary:false ~reg_vm:false ~jit:false ~llvm:false ~scheme:false ~aot:false ~output:(Some out) filename
  | [_; "compile"; "--wasm"; "--output"; out; filename] -> compile_file ~wasm:true ~wasm_binary:false ~reg_vm:false ~jit:false ~llvm:false ~scheme:false ~aot:false ~output:(Some out) filename
  | [_; "compile"; "--wasm-bin"; "--output"; out; filename] -> compile_file ~wasm:false ~wasm_binary:true ~reg_vm:false ~jit:false ~llvm:false ~scheme:false ~aot:false ~output:(Some out) filename
  | [_; "compile"; "--reg-vm"; "--output"; out; filename] -> compile_file ~wasm:false ~wasm_binary:false ~reg_vm:true ~jit:false ~llvm:false ~scheme:false ~aot:false ~output:(Some out) filename
  | [_; "compile"; "--jit"; "--output"; out; filename] -> compile_file ~wasm:false ~wasm_binary:false ~reg_vm:false ~jit:true ~llvm:false ~scheme:false ~aot:false ~output:(Some out) filename
  | [_; "compile"; "--llvm"; "--output"; out; filename] -> compile_file ~wasm:false ~wasm_binary:false ~reg_vm:false ~jit:false ~llvm:true ~scheme:false ~aot:false ~output:(Some out) filename
  | [_; "compile"; "--scheme"; "--output"; out; filename] -> compile_file ~wasm:false ~wasm_binary:false ~reg_vm:false ~jit:false ~llvm:false ~scheme:true ~aot:false ~output:(Some out) filename
  | [_; "compile"; "--aot"; filename] -> compile_file ~wasm:false ~wasm_binary:false ~reg_vm:false ~jit:false ~llvm:false ~scheme:false ~aot:true ~output:None filename
  | [_; "compile"; "--aot"; "--output"; out; filename] -> compile_file ~wasm:false ~wasm_binary:false ~reg_vm:false ~jit:false ~llvm:false ~scheme:false ~aot:true ~output:(Some out) filename
  | [_; "compile"; "--aot"; filename; "--output"; out] -> compile_file ~wasm:false ~wasm_binary:false ~reg_vm:false ~jit:false ~llvm:false ~scheme:false ~aot:true ~output:(Some out) filename
  | [_; "debug"; filename] -> debug_file filename
  | [_; "compile"; filename; "--output"; out] -> compile_file ~wasm:false ~wasm_binary:false ~reg_vm:false ~jit:false ~llvm:false ~scheme:false ~aot:false ~output:(Some out) filename
  | [_; "compile"; "--wasm"; filename; "--output"; out] -> compile_file ~wasm:true ~wasm_binary:false ~reg_vm:false ~jit:false ~llvm:false ~scheme:false ~aot:false ~output:(Some out) filename
  | [_; "compile"; "--wasm-bin"; filename; "--output"; out] -> compile_file ~wasm:false ~wasm_binary:true ~reg_vm:false ~jit:false ~llvm:false ~scheme:false ~aot:false ~output:(Some out) filename
  | [_; "compile"; "--reg-vm"; filename; "--output"; out] -> compile_file ~wasm:false ~wasm_binary:false ~reg_vm:true ~jit:false ~llvm:false ~scheme:false ~aot:false ~output:(Some out) filename
  | [_; "compile"; "--jit"; filename; "--output"; out] -> compile_file ~wasm:false ~wasm_binary:false ~reg_vm:false ~jit:true ~llvm:false ~scheme:false ~aot:false ~output:(Some out) filename
  | [_; "compile"; "--llvm"; filename; "--output"; out] -> compile_file ~wasm:false ~wasm_binary:false ~reg_vm:false ~jit:false ~llvm:true ~scheme:false ~aot:false ~output:(Some out) filename
  | [_; "compile"; "--scheme"; filename; "--output"; out] -> compile_file ~wasm:false ~wasm_binary:false ~reg_vm:false ~jit:false ~llvm:false ~scheme:true ~aot:false ~output:(Some out) filename
  | [_; "init"; name] -> Package_manager.init_project name
  | [_; "build"] ->
      if not (incremental_build_project ~parallel:false) then exit 1
  | [_; "build"; "--parallel"] ->
      if not (incremental_build_project ~parallel:true) then exit 1
  | [_; "build"; "--no-cache"] ->
      Compilation_cache.clear_all_cache ();
      if not (incremental_build_project ~parallel:false) then exit 1
  | [_; "build"; "--parallel"; "--no-cache"] | [_; "build"; "--no-cache"; "--parallel"] ->
      Compilation_cache.clear_all_cache ();
      if not (incremental_build_project ~parallel:true) then exit 1
  | [_; "deps"; filename] ->
      My_lang.Incremental_compile.show_dependency_graph filename
  | [_; "install"] -> Package_manager.install_dependencies ()
  | [_; "test"] -> Package_manager.run_tests ()
  | [_; "info"] ->
      let config = Package_manager.read_config () in
      print_endline (Package_manager.string_of_config config)
  | [_; "search"; query] ->
      let results = Registry.search_packages query in
      List.iter results ~f:Registry.show_package_info
  | [_; "add"; package_spec] ->
      (match String.lsplit2 package_spec ~on:'@' with
       | Some (name, version) -> Registry.install_package name version
       | None -> Registry.install_package package_spec "latest")
  | [_; "list"] ->
      Registry.list_installed ()
  | [_; "publish"] ->
      Registry.publish_package ()
  | [_; "lsp"] -> Lsp_server.start ()
  | [_; filename] -> run_file filename
  | _ ->
      print_endline "用法错误";
      print_usage ();
      exit 1
