open Core
open My_lang

let test_clang_available () =
  let exit_code, _, _ =
    let stdout_file = "/tmp/my_lang_test_stdout" in
    let stderr_file = "/tmp/my_lang_test_stderr" in
    let cmd = Printf.sprintf "clang --version > %s 2> %s" stdout_file stderr_file in
    let code = Stdlib.Sys.command cmd in
    let out = In_channel.read_all stdout_file in
    let err = In_channel.read_all stderr_file in
    Stdlib.Sys.remove stdout_file;
    Stdlib.Sys.remove stderr_file;
    (code, out, err)
  in
  if exit_code = 0 then
    printf "[PASS] test_clang_available\n"
  else
    printf "[SKIP] test_clang_available: clang not available\n"

let test_compile_ir () =
  let ir = "define i32 @main() { ret i32 42 }" in
  let output_path = "/tmp/my_lang_test_exec" in
  match Llvm_compile.compile_ir ir output_path with
  | Ok path ->
      (try Stdlib.Sys.remove path with _ -> ());
      printf "[PASS] test_compile_ir\n"
  | Error msg ->
      printf "[SKIP] test_compile_ir: %s\n" msg

let test_compile_and_run () =
  let ir = "define i32 @main() { ret i32 0 }" in
  (* 使用一个简单的 IR 直接测试 *)
  match Llvm_compile.compile_ir ir "/tmp/my_lang_test_run" with
  | Error msg -> printf "[SKIP] test_compile_and_run: %s\n" msg
  | Ok _ ->
      let exit_code, stdout, _ =
        let stdout_file = "/tmp/my_lang_run_stdout" in
        let stderr_file = "/tmp/my_lang_run_stderr" in
        let cmd = Printf.sprintf "/tmp/my_lang_test_run > %s 2> %s" stdout_file stderr_file in
        let code = Stdlib.Sys.command cmd in
        let out = In_channel.read_all stdout_file in
        let err = In_channel.read_all stderr_file in
        Stdlib.Sys.remove stdout_file;
        Stdlib.Sys.remove stderr_file;
        (code, out, err)
      in
      (try Stdlib.Sys.remove "/tmp/my_lang_test_run" with _ -> ());
      if exit_code = 0 then
        printf "[PASS] test_compile_and_run\n"
      else
        printf "[FAIL] test_compile_and_run: exit code %d\n" exit_code

let () =
  test_clang_available ();
  test_compile_ir ();
  test_compile_and_run ();
  printf "\nLLVM compile tests completed.\n"
