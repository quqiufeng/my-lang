(** 标准库测试 - 全面版 v2 *)
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
  Printf.printf "=== 标准库测试 v2 ===\n";
  
  (* 基础算术 *)
  run_test "加法" "1 + 2" "3";
  run_test "减法" "10 - 3" "7";
  run_test "乘法" "4 * 5" "20";
  run_test "除法" "15 / 3" "5";
  
  (* 比较 - 使用正确的语法 *)
  run_test "大于" "5 > 3" "true";
  run_test "小于" "3 < 5" "true";
  run_test "大于等于" "5 >= 5" "true";
  run_test "小于等于" "3 <= 5" "true";
  
  (* 字符串 *)
  run_test "字符串长度" "string_length \"hello\"" "5";
  run_test "字符串大写" "string_uppercase \"hello\"" "\"HELLO\"";
  run_test "字符串小写" "string_lowercase \"HELLO\"" "\"hello\"";
  run_test "字符串修剪" "string_trim \"  hello  \"" "\"hello\"";
  run_test "字符串开始于" "string_starts_with (\"hello\", \"hel\")" "true";
  run_test "字符串结束于" "string_ends_with (\"hello\", \"lo\")" "true";
  run_test "字符串拼接" "string_concat (\"\", [\"hello\", \" \", \"world\"])" "\"hello world\"";
  
  (* 数学 - 使用柯里化 *)
  run_test "绝对值" "math_abs (-5)" "5";
  run_test "平方根" "math_sqrt 9" "3";
  run_test "幂" "math_pow (2, 10)" "1024";
  run_test "求和" "math_sum [1, 2, 3, 4, 5]" "15";
  run_test "求积" "math_product [1, 2, 3, 4, 5]" "120";
  
  (* 类型检查 *)
  run_test "是整数" "is_int 42" "true";
  run_test "是字符串" "is_string \"hello\"" "true";
  run_test "是布尔值" "is_bool true" "true";
  run_test "是列表" "is_list [1, 2, 3]" "true";
  run_test "是函数" "is_function (fun x -> x)" "true";
  
  (* 转换 *)
  run_test "字符串转整数" "int_of_string \"42\"" "42";
  run_test "整数转字符串" "string_of_int 42" "\"42\"";
  run_test "整数转字符" "char_of_int 65" "'A'";
  run_test "字符转整数" "int_of_char 'A'" "65";
  
  (* 列表操作 *)
  run_test "头元素" "head [1, 2, 3]" "1";
  run_test "尾列表" "tail [1, 2, 3]" "[2, 3]";
  run_test "列表长度" "length [1, 2, 3]" "3";
  run_test "列表反转" "reverse [1, 2, 3]" "[3, 2, 1]";
  run_test "列表映射" "map (fun x -> x * 2) [1, 2, 3]" "[2, 4, 6]";
  run_test "列表过滤" "filter (fun x -> x > 2) [1, 2, 3, 4, 5]" "[3, 4, 5]";
  run_test "列表折叠" "fold (fun acc -> fun x -> acc + x) 0 [1, 2, 3, 4, 5]" "15";
  run_test "列表范围" "range 1 5" "[1, 2, 3, 4, 5]";
  run_test "列表排序" "sort [3, 1, 4, 1, 5]" "[1, 1, 3, 4, 5]";
  
  (* Let 绑定和函数 *)
  run_test "let 绑定" "let x = 10 in x + 5" "15";
  run_test "函数定义" "let f = fun x -> x * 2 in f 21" "42";
  run_test "函数组合" "let double = fun x -> x * 2 in let add_one = fun x -> x + 1 in add_one (double 5)" "11";
  
  (* 递归 *)
  run_test "递归 fib" "let rec fib = fun n -> if n <= 1 then n else fib (n - 1) + fib (n - 2) in fib 10" "55";
  run_test "递归 fact" "let rec fact = fun n -> if n <= 1 then 1 else n * fact (n - 1) in fact 5" "120";
  run_test "递归 sum" "let rec sum = fun n -> if n <= 0 then 0 else n + sum (n - 1) in sum 10" "55";
  
  (* 条件 *)
  run_test "if true" "if true then 1 else 0" "1";
  run_test "if false" "if false then 1 else 0" "0";
  run_test "if 比较" "if 5 > 3 then 100 else 200" "100";
  
  (* 高阶函数 - 使用元组参数 *)
  run_test "存在" "exists ((fun x -> x > 3), [1, 2, 3, 4, 5])" "true";
  run_test "全部" "forall ((fun x -> x > 0), [1, 2, 3, 4, 5])" "true";
  run_test "查找" "find ((fun x -> x > 3), [1, 2, 3, 4, 5])" "Some 4";
  run_test "取" "take (3, [1, 2, 3, 4, 5])" "[1, 2, 3]";
  run_test "丢" "drop (2, [1, 2, 3, 4, 5])" "[3, 4, 5]";
  
  (* Scheme 后端特有 *)
  run_test "ADT 定义" "type color = Red | Green | Blue; Red" "Red";
  run_test "ADT 构造" "type option = Some of int | None; Some 42" "Some 42";
  run_test "模式匹配" "type option = Some of int | None; match Some 42 with | Some n -> n | None -> 0" "42";
  
  Printf.printf "\n测试完成\n"
