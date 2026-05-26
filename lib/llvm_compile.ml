(** LLVM 可执行文件生成

    将 LLVM IR 文本编译为可执行文件。
    使用 clang 直接编译 LLVM IR（无需 llc）。
*)

open Core

exception LLVMCompileError of string

(** 执行 shell 命令并捕获输出 *)
let run_command cmd =
  let stdout_file = "/tmp/my_lang_stdout_" ^ string_of_int (Random.int 100000) in
  let stderr_file = "/tmp/my_lang_stderr_" ^ string_of_int (Random.int 100000) in
  let full_cmd = Printf.sprintf "%s > %s 2> %s" cmd stdout_file stderr_file in
  let exit_code = Stdlib.Sys.command full_cmd in
  let stdout = In_channel.read_all stdout_file in
  let stderr = In_channel.read_all stderr_file in
  Stdlib.Sys.remove stdout_file;
  Stdlib.Sys.remove stderr_file;
  (exit_code, stdout, stderr)

(** 检查 clang 是否可用 *)
let check_clang () =
  let exit_code, _, _ = run_command "clang --version" in
  if exit_code = 0 then Ok ()
  else Error "clang 未安装或不可用"

(** 编译 LLVM IR 字符串为可执行文件 *)
let compile_ir ?(opt_level="0") ir_text output_path =
  match check_clang () with
  | Error msg -> Error msg
  | Ok () ->
      let tmp_file = "/tmp/my_lang_" ^ string_of_int (Random.int 100000) ^ ".ll" in
      try
        Out_channel.write_all tmp_file ~data:ir_text;
        let cmd = Printf.sprintf "clang -x ir -O%s %s -o %s" opt_level tmp_file output_path in
        let exit_code, _, stderr_output = run_command cmd in
        Stdlib.Sys.remove tmp_file;
        if exit_code = 0 then
          Ok output_path
        else
          Error (Printf.sprintf "clang 编译失败 (退出码 %d): %s" exit_code stderr_output)
      with
      | exn ->
          (try Stdlib.Sys.remove tmp_file with _ -> ());
          Error (Printf.sprintf "编译异常: %s" (Exn.to_string exn))

(** 从寄存器字节码直接编译为可执行文件 *)
let compile_program ?(opt_level="0") prog output_path =
  let ir = Llvm_backend.generate_llvm_ir prog in
  compile_ir ~opt_level ir output_path

(** 编译并运行生成的可执行文件 *)
let compile_and_run ?(opt_level="0") prog =
  let output_path = "/tmp/my_lang_run_" ^ string_of_int (Random.int 100000) in
  match compile_program ~opt_level prog output_path with
  | Error msg -> Error msg
  | Ok path ->
      let exit_code, stdout, _ = run_command path in
      (try Stdlib.Sys.remove path with _ -> ());
      Ok (exit_code, stdout)
