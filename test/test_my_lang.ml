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
  (* 基础测试 *)
  run_test "integer" "42" (function VInt 42 -> true | _ -> false);
  run_test "arithmetic precedence" "1 + 2 * 3" (function VInt 7 -> true | _ -> false);
  run_test "let binding" "let x = 10 in x + 5" (function VInt 15 -> true | _ -> false);
  run_test "boolean logic" "true && false" (function VBool false -> true | _ -> false);
  run_test "if expression" "if 1 < 2 then 100 else 200" (function VInt 100 -> true | _ -> false);
  run_test "function application" "let f = fun x -> x + 1 in f 5" (function VInt 6 -> true | _ -> false);
  run_error_test "division by zero" "1 / 0";
  run_error_test "unbound variable" "x + 1";

  (* 字符串测试 *)
  run_test "string literal" "\"hello\"" (function VString "hello" -> true | _ -> false);
  run_test "string equality" "\"hello\" = \"hello\"" (function VBool true -> true | _ -> false);
  run_test "string comparison" "\"a\" < \"b\"" (function VBool true -> true | _ -> false);
  run_test "string concat" "\"hello\" ^ \" \" ^ \"world\"" (function VString "hello world" -> true | _ -> false);

  (* 列表测试 *)
  run_test "empty list" "[]" (function VList [] -> true | _ -> false);
  run_test "list literal" "[1, 2, 3]" (function VList [VInt 1; VInt 2; VInt 3] -> true | _ -> false);
  run_test "cons operator" "1 :: [2, 3]" (function VList [VInt 1; VInt 2; VInt 3] -> true | _ -> false);
  run_test "nested list" "[[1, 2], [3, 4]]" (function 
    VList [VList [VInt 1; VInt 2]; VList [VInt 3; VInt 4]] -> true | _ -> false);

  (* 元组测试 *)
  run_test "tuple" "(1, true, \"hello\")" (function 
    VTuple [VInt 1; VBool true; VString "hello"] -> true | _ -> false);
  run_test "empty tuple" "()" (function VTuple [] -> true | _ -> false);

  (* let rec 测试 *)
  run_test "let rec factorial" 
    "let rec factorial = fun n -> if n = 0 then 1 else n * factorial (n - 1) in factorial 5"
    (function VInt 120 -> true | _ -> false);

  run_test "let rec fibonacci" 
    "let rec fib = fun n -> if n <= 1 then n else fib (n - 1) + fib (n - 2) in fib 10"
    (function VInt 55 -> true | _ -> false);

  (* 顺序执行测试 *)
  run_test "sequence" "1; 2; 3" (function VInt 3 -> true | _ -> false);
  run_test "sequence with parens" "(let x = 1 in x + 1); 3" (function VInt 3 -> true | _ -> false);

  (* 内置函数测试 *)
  run_test "builtin head" "head [1, 2, 3]" (function VInt 1 -> true | _ -> false);
  run_test "builtin tail" "tail [1, 2, 3]" (function VList [VInt 2; VInt 3] -> true | _ -> false);
  run_test "builtin length list" "length [1, 2, 3]" (function VInt 3 -> true | _ -> false);
  run_test "builtin length string" "length \"hello\"" (function VInt 5 -> true | _ -> false);
  run_test "builtin print" "print \"hello\"" (function VUnit -> true | _ -> false);

  (* 模式匹配测试 *)
  run_test "match wildcard" "match 42 with | _ -> 100" (function VInt 100 -> true | _ -> false);
  
  run_test "match int" "match 42 with | 0 -> 1 | 42 -> 2 | _ -> 3" (function VInt 2 -> true | _ -> false);
  
  run_test "match bool" "match true with | true -> 1 | false -> 0" (function VInt 1 -> true | _ -> false);
  
  run_test "match string" "match \"hello\" with | \"world\" -> 1 | \"hello\" -> 2 | _ -> 3"
    (function VInt 2 -> true | _ -> false);
  
  run_test "match var" "match 42 with | x -> x + 1" (function VInt 43 -> true | _ -> false);
  
  run_test "match list" "match [1, 2, 3] with | [] -> 0 | [x] -> x | [x, y] -> x + y | [x, y, z] -> x + y + z"
    (function VInt 6 -> true | _ -> false);
  
  run_test "match tuple" "match (1, 2) with | (x, y) -> x + y" (function VInt 3 -> true | _ -> false);
  
  run_test "match cons" "match [1, 2, 3] with | [] -> 0 | h :: t -> h + length t"
    (function VInt 3 -> true | _ -> false);
  
  run_test "match nested" "match [[1, 2], [3, 4]] with | [a :: _, b :: _] -> a + b | _ -> 0"
    (function VInt 4 -> true | _ -> false);

  printf "\nAll tests completed.\n"
