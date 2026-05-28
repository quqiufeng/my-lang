open Alcotest
open My_lang

let test_int_arithmetic () =
  let result = My_lang.run ~check_ownership:false "1 + 2" in
  check string "1 + 2 = 3" "3" (Ast.string_of_value result)

let test_let_binding () =
  let result = My_lang.run ~check_ownership:false "let x = 42 in x" in
  check string "let x = 42 in x" "42" (Ast.string_of_value result)

let test_function () =
  let result = My_lang.run ~check_ownership:false "let f = fun x -> x + 1 in f 41" in
  check string "f 41 = 42" "42" (Ast.string_of_value result)

let test_recursion () =
  let result = My_lang.run ~check_ownership:false "let rec fib = fun n -> if n <= 1 then n else fib (n - 1) + fib (n - 2) in fib 10" in
  check string "fib 10 = 55" "55" (Ast.string_of_value result)

let test_list () =
  let result = My_lang.run ~check_ownership:false "length [1, 2, 3]" in
  check string "length [1,2,3] = 3" "3" (Ast.string_of_value result)

let test_string () =
  let result = My_lang.run ~check_ownership:false "string_length \"hello\"" in
  check string "string_length hello = 5" "5" (Ast.string_of_value result)

let test_hashmap () =
  let result = My_lang.run ~check_ownership:false "hashmap_size (hashmap_set (hashmap_create (), \"x\", 42))" in
  check string "hashmap_size = 1" "1" (Ast.string_of_value result)

let test_json () =
  let result = My_lang.run ~check_ownership:false "json_parse \"42\"" in
  check string "json_parse 42 = 42" "42" (Ast.string_of_value result)

let test_error_division_by_zero () =
  try
    ignore (My_lang.run ~check_ownership:false "1 / 0");
    fail "expected error"
  with _ -> ()

let test_error_unbound_variable () =
  try
    ignore (My_lang.run ~check_ownership:false "x");
    fail "expected error"
  with _ -> ()

let () =
  Alcotest.run "MyLang" [
    "arithmetic", [
      test_case "integer arithmetic" `Quick test_int_arithmetic;
    ];
    "binding", [
      test_case "let binding" `Quick test_let_binding;
    ];
    "functions", [
      test_case "function application" `Quick test_function;
      test_case "recursion" `Quick test_recursion;
    ];
    "data structures", [
      test_case "list operations" `Quick test_list;
      test_case "string operations" `Quick test_string;
      test_case "hashmap operations" `Quick test_hashmap;
    ];
    "json", [
      test_case "json parsing" `Quick test_json;
    ];
    "errors", [
      test_case "division by zero" `Quick test_error_division_by_zero;
      test_case "unbound variable" `Quick test_error_unbound_variable;
    ];
  ]
