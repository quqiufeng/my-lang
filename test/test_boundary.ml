open Core
open My_lang

let test_count = ref 0
let pass_count = ref 0

let test name code expected =
  incr test_count;
  try
    let result = My_lang.run code in
    let result_str = Ast.string_of_value result in
    if String.equal result_str expected then begin
      incr pass_count;
      Printf.printf "  PASS: %s\n" name
    end else
      Printf.printf "  FAIL: %s (expected %s, got %s)\n" name expected result_str
  with exn ->
    Printf.printf "  FAIL: %s (exception: %s)\n" name (Exn.to_string exn)

let test_error name code =
  incr test_count;
  try
    let _ = My_lang.run code in
    Printf.printf "  FAIL: %s (expected error)\n" name
  with _ ->
    incr pass_count;
    Printf.printf "  PASS: %s (error raised)\n" name

let () =
  Printf.printf "=== Boundary and Error Handling Tests ===\n\n";
  
  (* ===== 整数边界 ===== *)
  Printf.printf "-- 整数边界 --\n";
  test "int_max" "2147483647" "2147483647";
  test "int_min" "-2147483648" "-2147483648";
  test "int_zero" "0" "0";
  test "int_negative" "-1" "-1";
  test_error "int_div_zero" "1 / 0";
  
  (* ===== 字符串边界 ===== *)
  Printf.printf "\n-- 字符串边界 --\n";
  test "string_empty" "\"\"" "\"\"";
  test "string_single" "\"a\"" "\"a\"";
  test_error "string_unterminated" "\"unterminated";
  
  (* ===== 列表边界 ===== *)
  Printf.printf "\n-- 列表边界 --\n";
  test "list_empty" "[]" "[]";
  test "list_single" "[1]" "[1]";
  test "list_nested" "[[1], [2]]" "[[1]; [2]]";
  test_error "head_empty" "head []";
  test_error "tail_empty" "tail []";
  test_error "index_negative" "[1].[-1]";
  test_error "index_out_of_bounds" "[1].[1]";
  
  (* ===== 函数边界 ===== *)
  Printf.printf "\n-- 函数边界 --\n";
  test "function_identity" "let f = fun x -> x in f 42" "42";
  test "function_constant" "let f = fun x -> 42 in f 1" "42";
  test "function_nested" "let f = fun x -> fun y -> x + y in f 1 2" "3";
  test "function_recursive" "let rec f = fun n -> if n <= 0 then 0 else n + f (n - 1) in f 10" "55";
  test_error "function_not_function" "1 2";
  
  (* ===== 模式匹配边界 ===== *)
  Printf.printf "\n-- 模式匹配边界 --\n";
  test "match_wildcard" "match 42 with | _ -> true" "true";
  test "match_variable" "match 42 with | x -> x" "42";
  test "match_tuple" "match (1, 2) with | (a, b) -> a + b" "3";
  test "match_list_empty" "match [] with | [] -> true | _ -> false" "true";
  test "match_list_cons" "match [1, 2] with | x :: rest -> x" "1";
  test "match_nested" "match (1, [2, 3]) with | (x, y :: rest) -> x + y" "3";
  test_error "match_failure" "match 1 with | 2 -> true";
  
  (* ===== 引用边界 ===== *)
  Printf.printf "\n-- 引用边界 --\n";
  test "ref_create" "let r = ref 42 in !r" "42";
  test "ref_assign" "let r = ref 0 in r := 42; !r" "42";
  test "ref_multiple" "let r = ref 0 in r := 1; r := 2; r := 3; !r" "3";
  test_error "deref_non_ref" "!1";
  test_error "assign_non_ref" "1 := 2";
  
  (* ===== 模块边界 ===== *)
  Printf.printf "\n-- 模块边界 --\n";
  test "module_single" "module M = struct let x = 42 end; M.x" "42";
  test "module_multiple" "module M = struct let x = 1; let y = 2 end; M.x + M.y" "3";
  test_error "module_field_not_found" "module M = struct let x = 1 end; M.y";
  test_error "open_non_module" "open 1";
  
  (* ===== 错误传播 ===== *)
  Printf.printf "\n-- 错误传播 --\n";
  test_error "error_in_let" "let x = 1 / 0 in x";
  test_error "error_in_function" "let f = fun x -> 1 / 0 in f 1";
  test_error "error_in_if" "if 1 / 0 then true else false";
  test_error "error_in_match" "match 1 / 0 with | _ -> true";
  test_error "error_in_list" "[1, 2, 1 / 0, 4]";
  test_error "error_in_tuple" "(1, 2, 1 / 0, 4)";
  
  (* ===== 类型错误 ===== *)
  Printf.printf "\n-- 类型错误 --\n";
  test_error "type_error_add_int_string" "1 + \"a\"";
  test_error "type_error_sub_int_string" "1 - \"a\"";
  test_error "type_error_mul_int_string" "1 * \"a\"";
  test_error "type_error_div_int_string" "1 / \"a\"";
  test_error "type_error_eq_int_string" "1 = \"a\"";
  test_error "type_error_lt_int_string" "1 < \"a\"";
  test_error "type_error_and_int_bool" "1 && true";
  test_error "type_error_or_int_bool" "1 || true";
  test_error "type_error_not_int" "not 1";
  test_error "type_error_if_int" "if 1 then true else false";
  test_error "type_error_cons_int_list" "1 :: 2";
  test_error "type_error_concat_int_string" "1 ^ \"a\"";
  
  (* ===== 未绑定变量 ===== *)
  Printf.printf "\n-- 未绑定变量 --\n";
  test_error "unbound_variable" "x";
  test_error "unbound_in_function" "let f = fun x -> y in f 1";
  test_error "unbound_in_let" "let x = y in x";
  
  (* ===== 递归边界 ===== *)
  Printf.printf "\n-- 递归边界 --\n";
  test "recursion_base_case" "let rec f = fun n -> if n <= 0 then 0 else f (n - 1) in f 0" "0";
  test "recursion_small" "let rec f = fun n -> if n <= 0 then 0 else n + f (n - 1) in f 5" "15";
  test "recursion_fibonacci" "let rec fib = fun n -> if n <= 1 then n else fib (n - 1) + fib (n - 2) in fib 10" "55";
  test "recursion_factorial" "let rec fact = fun n -> if n <= 1 then 1 else n * fact (n - 1) in fact 10" "3628800";
  
  (* ===== 高阶函数边界 ===== *)
  Printf.printf "\n-- 高阶函数边界 --\n";
  test "higher_order_map" "map (fun x -> x + 1) [1, 2, 3]" "[2; 3; 4]";
  test "higher_order_filter" "filter (fun x -> x > 1) [1, 2, 3]" "[2; 3]";
  test "higher_order_fold" "fold (fun acc -> fun x -> acc + x) 0 [1, 2, 3]" "6";
  
  Printf.printf "\n=== Results: %d/%d passed ===\n" !pass_count !test_count
