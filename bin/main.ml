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
  print_endline "";
  print_endline "包管理:";
  print_endline "  my_lang init <name>        初始化新项目";
  print_endline "  my_lang build              增量构建项目";
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

let compile_file ~wasm ~wasm_binary ~reg_vm ~jit ~output filename =
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

let incremental_build_project () =
  let config = Package_manager.read_config () in
  Printf.printf "Building project '%s' v%s (incremental)...\n" config.name config.version;
  
  let entry = Option.value config.entry_point ~default:"main.ml" in
  if not (Stdlib.Sys.file_exists entry) then begin
    Printf.eprintf "Entry point '%s' not found\n" entry;
    false
  end else
    match My_lang.Incremental_compile.compile_and_link ~cache:true entry with
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
  | [_; "compile"; filename] -> compile_file ~wasm:false ~wasm_binary:false ~reg_vm:false ~jit:false ~output:None filename
  | [_; "compile"; "--wasm"; filename] -> compile_file ~wasm:true ~wasm_binary:false ~reg_vm:false ~jit:false ~output:None filename
  | [_; "compile"; "--wasm-bin"; filename] -> compile_file ~wasm:false ~wasm_binary:true ~reg_vm:false ~jit:false ~output:None filename
  | [_; "compile"; "--reg-vm"; filename] -> compile_file ~wasm:false ~wasm_binary:false ~reg_vm:true ~jit:false ~output:None filename
  | [_; "compile"; "--jit"; filename] -> compile_file ~wasm:false ~wasm_binary:false ~reg_vm:false ~jit:true ~output:None filename
  | [_; "compile"; "--output"; out; filename] -> compile_file ~wasm:false ~wasm_binary:false ~reg_vm:false ~jit:false ~output:(Some out) filename
  | [_; "compile"; "--wasm"; "--output"; out; filename] -> compile_file ~wasm:true ~wasm_binary:false ~reg_vm:false ~jit:false ~output:(Some out) filename
  | [_; "compile"; "--wasm-bin"; "--output"; out; filename] -> compile_file ~wasm:false ~wasm_binary:true ~reg_vm:false ~jit:false ~output:(Some out) filename
  | [_; "compile"; "--reg-vm"; "--output"; out; filename] -> compile_file ~wasm:false ~wasm_binary:false ~reg_vm:true ~jit:false ~output:(Some out) filename
  | [_; "compile"; "--jit"; "--output"; out; filename] -> compile_file ~wasm:false ~wasm_binary:false ~reg_vm:false ~jit:true ~output:(Some out) filename
  | [_; "compile"; filename; "--output"; out] -> compile_file ~wasm:false ~wasm_binary:false ~reg_vm:false ~jit:false ~output:(Some out) filename
  | [_; "compile"; "--wasm"; filename; "--output"; out] -> compile_file ~wasm:true ~wasm_binary:false ~reg_vm:false ~jit:false ~output:(Some out) filename
  | [_; "compile"; "--wasm-bin"; filename; "--output"; out] -> compile_file ~wasm:false ~wasm_binary:true ~reg_vm:false ~jit:false ~output:(Some out) filename
  | [_; "compile"; "--reg-vm"; filename; "--output"; out] -> compile_file ~wasm:false ~wasm_binary:false ~reg_vm:true ~jit:false ~output:(Some out) filename
  | [_; "compile"; "--jit"; filename; "--output"; out] -> compile_file ~wasm:false ~wasm_binary:false ~reg_vm:false ~jit:true ~output:(Some out) filename
  | [_; "init"; name] -> Package_manager.init_project name
  | [_; "build"] ->
      if not (incremental_build_project ()) then exit 1
  | [_; "build"; "--no-cache"] ->
      Compilation_cache.clear_all_cache ();
      if not (incremental_build_project ()) then exit 1
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
