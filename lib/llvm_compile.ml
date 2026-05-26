(** LLVM 可执行文件生成

    将 LLVM IR 文本编译为可执行文件。
    使用 clang 直接编译 LLVM IR（无需 llc）。
*)

open Core

exception LLVMCompileError of string

(** 检查 clang 是否可用 *)
let check_clang () =
  let exit_code, _, _ = Resource_manager.run_command_with_cleanup "clang --version" in
  if exit_code = 0 then Ok ()
  else Error "clang 未安装或不可用"

(** 编译 LLVM IR 字符串为可执行文件 *)
let compile_ir ?(opt_level="0") ir_text output_path =
  match check_clang () with
  | Error msg -> Error msg
  | Ok () ->
      Resource_manager.with_temp_file "my_lang" ".ll" (fun tmp_file ->
        try
          Out_channel.write_all tmp_file ~data:ir_text;
          let cmd = Printf.sprintf "clang -x ir -O%s %s -o %s" opt_level tmp_file output_path in
          let exit_code, _, stderr_output = Resource_manager.run_command_with_cleanup cmd in
          if exit_code = 0 then
            Ok output_path
          else
            Error (Printf.sprintf "clang 编译失败 (退出码 %d): %s" exit_code stderr_output)
        with
        | exn ->
            Error (Printf.sprintf "编译异常: %s" (Exn.to_string exn))
      )

(** 从寄存器字节码直接编译为可执行文件 *)
let compile_program ?(opt_level="0") prog output_path =
  let ir = Llvm_backend.generate_llvm_ir prog in
  compile_ir ~opt_level ir output_path

(** 编译并运行生成的可执行文件 *)
let compile_and_run ?(opt_level="0") prog =
  Resource_manager.with_temp_file "my_lang_run" "" (fun output_path ->
    match compile_program ~opt_level prog output_path with
    | Error msg -> Error msg
    | Ok path ->
        let exit_code, stdout, _ = Resource_manager.run_command_with_cleanup path in
        Ok (exit_code, stdout)
  )
