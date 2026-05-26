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

  (* 类型推断错误测试 *)
  run_error_test "type error: string + int" "\"hello\" + 1";
  run_error_test "type error: bool + int" "true + 1";
  run_error_test "type error: if branches differ" "if true then 1 else \"hello\"";
  run_error_test "type error: apply non-function" "1 2";
  run_error_test "type error: list hetero" "[1, \"hello\"]";

  (* while 循环测试 *)
  run_test "while false" "while false do 42 done" (function VUnit -> true | _ -> false);

  (* 索引访问测试 *)
  run_test "list index" "[10, 20, 30][1]" (function VInt 20 -> true | _ -> false);
  run_test "string index" "\"hello\"[1]" (function VString "e" -> true | _ -> false);
  run_error_test "index out of bounds" "[1, 2][5]";

  (* 切片测试 *)
  run_test "list slice" "[1, 2, 3, 4, 5][1:3]" (function VList [VInt 2; VInt 3] -> true | _ -> false);
  run_test "string slice" "\"hello\"[1:4]" (function VString "ell" -> true | _ -> false);
  run_test "list slice full" "[1, 2, 3][0:5]" (function VList [VInt 1; VInt 2; VInt 3] -> true | _ -> false);

  (* show 内置函数 *)
  run_test "show int" "show 42" (function VString "42" -> true | _ -> false);
  run_test "show bool" "show true" (function VString "true" -> true | _ -> false);
  run_test "show list" "show [1, 2]" (function VString "[1; 2]" -> true | _ -> false);

  (* 高阶函数测试 *)
  run_test "map add1" "map (fun x -> x + 1) [1, 2, 3]" (function VList [VInt 2; VInt 3; VInt 4] -> true | _ -> false);
  run_test "filter even" "filter (fun x -> x > 1) [1, 2, 3]" (function VList [VInt 2; VInt 3] -> true | _ -> false);
  run_test "fold sum" "fold (fun acc -> fun x -> acc + x) 0 [1, 2, 3]" (function VInt 6 -> true | _ -> false);

  (* ADT 测试 *)
  run_test "adt enum" "type color = Red | Green | Blue; Red" (function VCtor ("Red", None) -> true | _ -> false);
  run_test "adt ctor with arg" "type option_int = Some of int | None; Some 42" (function VCtor ("Some", Some (VInt 42)) -> true | _ -> false);
  run_test "adt match" "type color = Red | Green | Blue; match Red with | Red -> 1 | Green -> 2 | Blue -> 3" (function VInt 1 -> true | _ -> false);
  run_test "adt match with arg" "type option_int = Some of int | None; match Some 42 with | Some x -> x | None -> 0" (function VInt 42 -> true | _ -> false);
  run_test "adt match wildcard" "type color = Red | Green; match Green with | Red -> 1 | _ -> 2" (function VInt 2 -> true | _ -> false);

  (* 引用类型测试 *)
  run_test "ref deref" "let x = ref 10 in !x" (function VInt 10 -> true | _ -> false);
  run_test "ref assign" "let x = ref 10 in x := 20; !x" (function VInt 20 -> true | _ -> false);
  run_test "ref counter" "let c = ref 0 in c := !c + 1; c := !c + 1; !c" (function VInt 2 -> true | _ -> false);
  run_error_test "deref non-ref" "!42";
  run_error_test "assign non-ref" "42 := 1";

  (* 异常处理测试 *)
  run_test "try catch" "try raise 42 with | x -> x" (function VInt 42 -> true | _ -> false);
  run_test "try no catch" "try 42 with | x -> x" (function VInt 42 -> true | _ -> false);
  run_test "try nested" "try (try raise 1 with | x -> x + 10) with | x -> x + 100" (function VInt 11 -> true | _ -> false);
  run_test "try pattern" "try raise true with | true -> 1 | false -> 0" (function VInt 1 -> true | _ -> false);

  (* 数组测试 *)
  run_test "array empty" "[||]" (function VArray a when Array.length a = 0 -> true | _ -> false);
  run_test "array literal" "[|1, 2, 3|]" (function VArray a when Array.length a = 3 -> true | _ -> false);
  run_test "array get" "let a = [|10, 20, 30|] in a.(1)" (function VInt 20 -> true | _ -> false);
  run_test "array set" "let a = [|10, 20, 30|] in a.(1) <- 99; a.(1)" (function VInt 99 -> true | _ -> false);
  run_error_test "array index out of bounds" "let a = [|1, 2|] in a.(5)";

  (* 字符类型测试 *)
  run_test "char literal" "'a'" (function VChar 'a' -> true | _ -> false);
  run_test "char newline" "'\\n'" (function VChar '\n' -> true | _ -> false);

  (* 字符串操作测试 *)
  run_test "string_length" "string_length \"hello\"" (function VInt 5 -> true | _ -> false);
  run_test "string_get" "string_get \"hello\" 1" (function VChar 'e' -> true | _ -> false);
  run_test "string_sub" "string_sub \"hello\" 1 3" (function VString "ell" -> true | _ -> false);
  run_error_test "string_get out of bounds" "string_get \"hi\" 5";

  (* 文件 IO 测试 *)
  run_test "write_and_read_file" 
    "let x = write_file \"/tmp/test_ml.txt\" \"hello world\" in read_file \"/tmp/test_ml.txt\""
    (function VString "hello world" -> true | _ -> false);
  run_test "print_string" "print_string \"test\""
    (function VUnit -> true | _ -> false);

  (* 记录类型测试 *)
  run_test "record empty" "{}" (function VRecord [] -> true | _ -> false);
  run_test "record literal" "{name = \"x\"; age = 1}" 
    (function VRecord ["name", {contents = VString "x"}; "age", {contents = VInt 1}] -> true | _ -> false);
  run_test "record get" "let p = {name = \"x\"; age = 1} in p.name" 
    (function VString "x" -> true | _ -> false);
  run_test "record set" "let p = {name = \"x\"; age = 1} in p.name <- \"y\"; p.name" 
    (function VString "y" -> true | _ -> false);
  run_error_test "record get missing field" "let p = {a = 1} in p.b";

  (* todo 语法糖测试 *)
  run_error_test "todo" "todo \"implement me\"";

  printf "\nAll tests completed.\n"
