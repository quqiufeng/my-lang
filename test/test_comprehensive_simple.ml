(** 标准库测试 - 全面版 *)
open My_lang

let run_test name code expected =
  try
    let result = My_lang.run code in
    let result_str = Ast.string_of_value result in
    if result_str = expected then
      Printf.printf "✅ %s: %s = %s\n" name code result_str
    else
      Printf.printf "❌ %s: 期望 %s, 得到 %s\n" name expected result_str
  with
  | exn -> Printf.printf "❌ %s: 异常 %s\n" name (Printexc.to_string exn)

let () =
  Printf.printf "=== 标准库测试 ===\n";
  
  (* 基础算术 *)
  run_test "加法" "1 + 2" "3";
  run_test "减法" "10 - 3" "7";
  run_test "乘法" "4 * 5" "20";
  run_test "除法" "15 / 3" "5";
  
  (* 比较 *)
  run_test "等于" "1 == 1" "true";
  run_test "不等于" "1 != 2" "true";
  run_test "大于" "5 > 3" "true";
  
  (* 字符串 *)
  run_test "字符串长度" "string_length \"hello\"" "5";
  run_test "字符串大写" "string_uppercase \"hello\"" "\"HELLO\"";
  run_test "字符串小写" "string_lowercase \"HELLO\"" "\"hello\"";
  run_test "字符串包含" "string_contains \"hello\" \"lo\"" "true";
  
  (* 数学 *)
  run_test "绝对值" "math_abs (-5)" "5";
  run_test "最大值" "math_max 3 7" "7";
  run_test "最小值" "math_min 3 7" "3";
  run_test "平方根" "math_sqrt 9" "3";
  
  (* 类型检查 *)
  run_test "是整数" "is_int 42" "true";
  run_test "是字符串" "is_string \"hello\"" "true";
  run_test "是布尔值" "is_bool true" "true";
  
  (* 转换 *)
  run_test "字符串转整数" "int_of_string \"42\"" "42";
  run_test "整数转字符串" "string_of_int 42" "\"42\"";
  
  (* Let 绑定 *)
  run_test "let 绑定" "let x = 10 in x + 5" "15";
  run_test "函数定义" "let f = fun x -> x * 2 in f 21" "42";
  
  (* 递归 *)
  run_test "递归 fib" "let rec fib = fun n -> if n <= 1 then n else fib (n - 1) + fib (n - 2) in fib 10" "55";
  run_test "递归 fact" "let rec fact = fun n -> if n <= 1 then 1 else n * fact (n - 1) in fact 5" "120";
  
  (* 条件 *)
  run_test "if true" "if true then 1 else 0" "1";
  run_test "if false" "if false then 1 else 0" "0";
  
  Printf.printf "\n测试完成\n"
