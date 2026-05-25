open Core
open My_lang
open My_lang.Ast

let run_test name code check =
  match My_lang.run_exn code with
  | Ok v when check v ->
      printf "[PASS] %s\n" name
  | Ok v ->
      printf "[FAIL] %s: got %s\n" name (string_of_value v)
  | Error msg ->
      printf "[FAIL] %s: unexpected error: %s\n" name msg

let run_error_test name code =
  match My_lang.run_exn code with
  | Ok v ->
      printf "[FAIL] %s: expected error, got %s\n" name (string_of_value v)
  | Error _ ->
      printf "[PASS] %s\n" name

let () =
  run_test "integer" "42" (function VInt 42 -> true | _ -> false);
  run_test "arithmetic precedence" "1 + 2 * 3" (function VInt 7 -> true | _ -> false);
  run_test "let binding" "let x = 10 in x + 5" (function VInt 15 -> true | _ -> false);
  run_test "boolean logic" "true && false" (function VBool false -> true | _ -> false);
  run_test "if expression" "if 1 < 2 then 100 else 200" (function VInt 100 -> true | _ -> false);
  run_test "function application" "let f = fun x -> x + 1 in f 5" (function VInt 6 -> true | _ -> false);
  run_error_test "division by zero" "1 / 0";
  run_error_test "unbound variable" "x + 1";
  printf "\nAll tests completed.\n"
