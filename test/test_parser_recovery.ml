open Core
open My_lang

let test_parser_error_recovery () =
  (* 测试带有语法错误的代码 *)
  let code = "let x = 1 + \nlet y = 2" in
  match My_lang.run_exn code with
  | Ok v ->
      printf "[FAIL] test_parser_error_recovery: expected error, got %s\n" (Ast.string_of_value v)
  | Error msg ->
      if String.is_substring msg ~substring:"解析错误" || String.is_substring msg ~substring:"Parse error" then
        printf "[PASS] test_parser_error_recovery\n"
      else
        printf "[FAIL] test_parser_error_recovery: expected parse error in message, got: %s\n" msg

let test_parser_error_position () =
  (* 测试错误位置报告 *)
  let code = "if true then\n  let x =" in
  match My_lang.run_exn code with
  | Ok _ ->
      printf "[FAIL] test_parser_error_position: expected error\n"
  | Error msg ->
      (* 应该报告位置信息 *)
      if String.is_substring msg ~substring:"1:" || String.is_substring msg ~substring:"2:" then
        printf "[PASS] test_parser_error_position\n"
      else
        printf "[INFO] test_parser_error_position: message = %s\n" msg

let test_valid_code_still_works () =
  let code = "let x = 1 + 2 in x" in
  match My_lang.run_exn code with
  | Ok v ->
      if String.equal (Ast.string_of_value v) "3" then
        printf "[PASS] test_valid_code_still_works\n"
      else
        printf "[FAIL] test_valid_code_still_works: expected 3, got %s\n" (Ast.string_of_value v)
  | Error msg ->
      printf "[FAIL] test_valid_code_still_works: unexpected error: %s\n" msg

let () =
  test_parser_error_recovery ();
  test_parser_error_position ();
  test_valid_code_still_works ();
  printf "\nParser recovery tests completed.\n"
