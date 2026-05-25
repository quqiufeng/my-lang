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
  print_endline "  my_lang compile --wasm <file>  编译为 WASM";
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

let compile_file ~wasm ~output filename =
  try
    let content = In_channel.read_all filename in
    let expr = My_lang.parse content in
    let _ = My_lang.typecheck expr in
    if wasm then
      let wasm_code = My_lang.compile_to_wasm expr in
      match output with
      | Some out ->
          Out_channel.write_all out ~data:wasm_code;
          Printf.printf "WASM 已写入: %s\n" out
      | None ->
          print_endline wasm_code
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

let () =
  let args = Array.to_list (Sys.get_argv ()) in
  match args with
  | [_] ->
      print_usage ();
      print_endline "";
      repl ()
  | [_; filename] -> run_file filename
  | [_; "compile"; filename] -> compile_file ~wasm:false ~output:None filename
  | [_; "compile"; "--wasm"; filename] -> compile_file ~wasm:true ~output:None filename
  | [_; "compile"; "--output"; out; filename] -> compile_file ~wasm:false ~output:(Some out) filename
  | [_; "compile"; "--wasm"; "--output"; out; filename] -> compile_file ~wasm:true ~output:(Some out) filename
  | [_; "compile"; filename; "--output"; out] -> compile_file ~wasm:false ~output:(Some out) filename
  | [_; "compile"; "--wasm"; filename; "--output"; out] -> compile_file ~wasm:true ~output:(Some out) filename
  | _ ->
      print_endline "用法错误";
      print_usage ();
      exit 1
