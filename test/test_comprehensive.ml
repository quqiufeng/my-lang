(** 全面的标准库测试套件
    使用 alcotest 框架进行结构化测试
*)

open Alcotest
open My_lang

(** 测试辅助函数 *)
let run_code code =
  My_lang.run code

let run_code_expect_int code expected =
  let result = run_code code in
  match result with
  | Ast.VInt n -> check int code expected n
  | _ -> failf "Expected VInt, got %s" (Ast.string_of_value result)

let run_code_expect_bool code expected =
  let result = run_code code in
  match result with
  | Ast.VBool b -> check bool code expected b
  | _ -> failf "Expected VBool, got %s" (Ast.string_of_value result)

let run_code_expect_string code expected =
  let result = run_code code in
  match result with
  | Ast.VString s -> check string code expected s
  | _ -> failf "Expected VString, got %s" (Ast.string_of_value result)

let run_code_expect_list code expected_len =
  let result = run_code code in
  match result with
  | Ast.VList l -> check int (code ^ " length") expected_len (List.length l)
  | _ -> failf "Expected VList, got %s" (Ast.string_of_value result)

(** 基础算术测试 *)
let test_arithmetic () =
  run_code_expect_int "1 + 2" 3;
  run_code_expect_int "10 - 3" 7;
  run_code_expect_int "4 * 5" 20;
  run_code_expect_int "15 / 3" 5;
  run_code_expect_int "17 % 5" 2;
  run_code_expect_int "2 + 3 * 4" 14;
  run_code_expect_int "(2 + 3) * 4" 20

(** 比较运算测试 *)
let test_comparison () =
  run_code_expect_bool "1 == 1" true;
  run_code_expect_bool "1 == 2" false;
  run_code_expect_bool "1 != 2" true;
  run_code_expect_bool "3 > 2" true;
  run_code_expect_bool "2 < 3" true;
  run_code_expect_bool "3 >= 3" true;
  run_code_expect_bool "2 <= 3" true

(** 布尔运算测试 *)
let test_boolean () =
  run_code_expect_bool "true && true" true;
  run_code_expect_bool "true && false" false;
  run_code_expect_bool "true || false" true;
  run_code_expect_bool "false || false" false;
  run_code_expect_bool "!true" false;
  run_code_expect_bool "!false" true

(** 字符串函数测试 *)
let test_string_functions () =
  run_code_expect_int "string_length \"hello\"" 5;
  run_code_expect_string "string_uppercase \"hello\"" "HELLO";
  run_code_expect_string "string_lowercase \"WORLD\"" "world";
  run_code_expect_string "string_trim \"  spaces  \"" "spaces";
  run_code_expect_bool "string_contains \"hello world\" \"world\"" true;
  run_code_expect_bool "string_contains \"hello\" \"xyz\"" false;
  run_code_expect_bool "string_starts_with \"hello\" \"hel\"" true;
  run_code_expect_bool "string_ends_with \"hello\" \"llo\"" true;
  run_code_expect_string "string_concat \"hello\" \" world\"" "hello world";
  run_code_expect_int "string_find \"hello world\" \"world\"" 6

(** 列表函数测试 *)
let test_list_functions () =
  run_code_expect_int "head [1; 2; 3]" 1;
  run_code_expect_list "tail [1; 2; 3]" 2;
  run_code_expect_int "length [1; 2; 3; 4; 5]" 5;
  run_code_expect_list "reverse [1; 2; 3]" 3;
  run_code_expect_list "append [1; 2] [3; 4]" 4;
  run_code_expect_list "take 3 [1; 2; 3; 4; 5]" 3;
  run_code_expect_list "drop 2 [1; 2; 3; 4; 5]" 3;
  run_code_expect_list "map (fun x -> x * 2) [1; 2; 3]" 3;
  run_code_expect_list "filter (fun x -> x > 2) [1; 2; 3; 4; 5]" 3

(** 数学函数测试 *)
let test_math_functions () =
  run_code_expect_int "math_abs (-5)" 5;
  run_code_expect_int "math_abs 10" 10;
  run_code_expect_int "math_max 3 7" 7;
  run_code_expect_int "math_min 3 7" 3;
  run_code_expect_int "math_sqrt 9" 3;
  run_code_expect_int "math_pow 2 10" 1024;
  run_code_expect_int "math_mod 17 5" 2;
  run_code_expect_int "math_gcd 12 8" 4;
  run_code_expect_int "math_lcm 4 6" 12;
  run_code_expect_int "math_sign (-5)" (-1);
  run_code_expect_int "math_sign 5" 1;
  run_code_expect_int "math_sign 0" 0

(** 类型检查函数测试 *)
let test_type_checks () =
  run_code_expect_bool "is_int 42" true;
  run_code_expect_bool "is_int \"hello\"" false;
  run_code_expect_bool "is_string \"hello\"" true;
  run_code_expect_bool "is_string 42" false;
  run_code_expect_bool "is_bool true" true;
  run_code_expect_bool "is_bool 42" false;
  run_code_expect_bool "is_list [1; 2; 3]" true;
  run_code_expect_bool "is_list 42" false;
  run_code_expect_bool "is_function (fun x -> x)" true;
  run_code_expect_bool "is_function 42" false

(** 转换函数测试 *)
let test_conversion_functions () =
  run_code_expect_int "int_of_string \"42\"" 42;
  run_code_expect_string "string_of_int 42" "42";
  run_code_expect_string "bool_to_string true" "true";
  run_code_expect_string "bool_to_string false" "false";
  run_code_expect_int "char_of_int 65" 65;
  run_code_expect_int "int_of_char 'A'" 65

(** 高阶函数测试 *)
let test_higher_order_functions () =
  run_code_expect_int "fold (fun acc x -> acc + x) 0 [1; 2; 3; 4; 5]" 15;
  run_code_expect_bool "exists (fun x -> x > 3) [1; 2; 3; 4; 5]" true;
  run_code_expect_bool "exists (fun x -> x > 10) [1; 2; 3; 4; 5]" false;
  run_code_expect_bool "forall (fun x -> x > 0) [1; 2; 3; 4; 5]" true;
  run_code_expect_bool "forall (fun x -> x > 3) [1; 2; 3; 4; 5]" false;
  run_code_expect_int "find (fun x -> x > 3) [1; 2; 3; 4; 5]" 4

(** Let 绑定和函数定义测试 *)
let test_let_and_functions () =
  run_code_expect_int "let x = 10 in x + 5" 15;
  run_code_expect_int "let x = 5 in let y = 10 in x + y" 15;
  run_code_expect_int "let f = fun x -> x * 2 in f 21" 42;
  run_code_expect_int "let add = fun x y -> x + y in add 3 4" 7

(** 递归函数测试 *)
let test_recursion () =
  run_code_expect_int 
    "let rec fib = fun n -> if n <= 1 then n else fib (n - 1) + fib (n - 2) in fib 10" 
    55;
  run_code_expect_int 
    "let rec fact = fun n -> if n <= 1 then 1 else n * fact (n - 1) in fact 5" 
    120;
  run_code_expect_int 
    "let rec sum = fun n -> if n <= 0 then 0 else n + sum (n - 1) in sum 10" 
    55

(** 条件表达式测试 *)
let test_conditionals () =
  run_code_expect_int "if true then 1 else 0" 1;
  run_code_expect_int "if false then 1 else 0" 0;
  run_code_expect_int "if 5 > 3 then 100 else 200" 100;
  run_code_expect_int "if 5 < 3 then 100 else 200" 200

(** 元组测试 *)
let test_tuples () =
  run_code_expect_int "let (x, y) = (1, 2) in x + y" 3;
  run_code_expect_int "fst (1, 2)" 1;
  run_code_expect_int "snd (1, 2)" 2

(** 引用测试 *)
let test_references () =
  run_code_expect_int "let r = ref 10 in !r" 10;
  run_code_expect_int "let r = ref 10 in r := 20; !r" 20

(** 异常处理测试 *)
let test_exceptions () =
  run_code_expect_int 
    "try (raise 42) with | e -> e" 
    42

(** 测试套件定义 *)
let arithmetic_tests = [
  test_case "基础算术" `Quick test_arithmetic;
  test_case "比较运算" `Quick test_comparison;
  test_case "布尔运算" `Quick test_boolean;
]

let stdlib_tests = [
  test_case "字符串函数" `Quick test_string_functions;
  test_case "列表函数" `Quick test_list_functions;
  test_case "数学函数" `Quick test_math_functions;
  test_case "类型检查" `Quick test_type_checks;
  test_case "转换函数" `Quick test_conversion_functions;
  test_case "高阶函数" `Quick test_higher_order_functions;
]

let language_tests = [
  test_case "Let绑定和函数" `Quick test_let_and_functions;
  test_case "递归函数" `Quick test_recursion;
  test_case "条件表达式" `Quick test_conditionals;
  test_case "元组" `Quick test_tuples;
  test_case "引用" `Quick test_references;
  test_case "异常处理" `Quick test_exceptions;
]

(** 主测试入口 *)
let () =
  Alcotest.run "MyLang 全面测试套件" [
    "算术和逻辑", arithmetic_tests;
    "标准库函数", stdlib_tests;
    "语言特性", language_tests;
  ]
