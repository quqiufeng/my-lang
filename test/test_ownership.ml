(** 所有权与借用检查测试 *)

open Core
open My_lang

let test_ownership name code expect_error =
  try
    let expr = My_lang.parse code in
    let env = Ownership.create_borrow_env () in
    Ownership.enter_scope env;
    Ownership.check_expr env expr;
    Ownership.exit_scope env;
    if expect_error then
      Printf.printf "[FAIL] %s: 应该报错但没有\n" name
    else
      Printf.printf "[PASS] %s\n" name
  with Ownership.OwnershipError msg ->
    if expect_error then
      Printf.printf "[PASS] %s: %s\n" name msg
    else
      Printf.printf "[FAIL] %s: 意外错误: %s\n" name msg

let () =
  test_ownership "simple let" "let x = 5 in x + 1" false;
  test_ownership "use moved var" "let x = 5 in let y = x in x + 1" true;
  test_ownership "function param" "let f = fun x -> x + 1 in f 5" false;
  test_ownership "if expr" "if true then 1 else 2" false;
  test_ownership "nested let" "let a = 1 in let b = 2 in a + b" false;
  Printf.printf "所有权检查测试完成\n"
