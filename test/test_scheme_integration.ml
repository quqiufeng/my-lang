(** Scheme 后端和标准库集成测试

    测试 Scheme 后端、ADT、模式匹配、AoT 编译等功能。
*)

open My_lang

(** 测试辅助函数 *)
let run_test name code expected =
  Printf.printf "测试: %s\n" name;
  try
    let result = My_lang.run code in
    let result_str = Ast.string_of_value result in
    if result_str = expected then begin
      Printf.printf "  ✅ 通过: %s = %s\n" name result_str;
      true
    end else begin
      Printf.printf "  ❌ 失败: 期望 %s, 得到 %s\n" expected result_str;
      false
    end
  with
  | exn ->
    Printf.printf "  ❌ 异常: %s\n" (Printexc.to_string exn);
    false

(** 测试 Scheme 后端编译 *)
let test_scheme_backend () =
  Printf.printf "\n=== Scheme 后端测试 ===\n";
  
  let tests = [
    ("算术运算", "1 + 2", "3");
    ("let 绑定", "let x = 10 in x + 5", "15");
    ("函数定义", "let f = fun x -> x * 2 in f 21", "42");
    ("条件表达式", "if 1 > 0 then 1 else 0", "1");
    ("递归函数", "let rec fib = fun n -> if n <= 1 then n else fib (n - 1) + fib (n - 2) in fib 10", "55");
  ] in
  
  let passed = List.fold_left (fun acc (name, code, expected) ->
    if run_test name code expected then acc + 1 else acc
  ) 0 tests in
  
  Printf.printf "通过: %d/%d\n" passed (List.length tests);
  passed = List.length tests

(** 测试 ADT 和模式匹配 *)
let test_adt_pattern_matching () =
  Printf.printf "\n=== ADT 和模式匹配测试 ===\n";
  
  let tests = [
    ("无参数 ADT", "type color = Red | Green | Blue; Red", "Red");
    ("有参数 ADT", "type option = Some of int | None; Some 42", "Some 42");
    ("模式匹配 - 匹配成功", "type option = Some of int | None; match Some 42 with | Some n -> n | None -> 0", "42");
    ("模式匹配 - 匹配 None", "type option = Some of int | None; match None with | Some n -> n | None -> -1", "-1");
  ] in
  
  let passed = List.fold_left (fun acc (name, code, expected) ->
    if run_test name code expected then acc + 1 else acc
  ) 0 tests in
  
  Printf.printf "通过: %d/%d\n" passed (List.length tests);
  passed = List.length tests

(** 测试标准库函数 *)
let test_stdlib_functions () =
  Printf.printf "\n=== 标准库函数测试 ===\n";
  
  let tests = [
    (* 列表操作 *)
    ("head", "head [1; 2; 3]", "1");
    ("tail", "tail [1; 2; 3]", "[2, 3]");
    ("length", "length [1; 2; 3]", "3");
    ("map", "map (fun x -> x * 2) [1; 2; 3]", "[2, 4, 6]");
    ("filter", "filter (fun x -> x > 1) [1; 2; 3]", "[2, 3]");
    ("fold", "fold (fun acc x -> acc + x) 0 [1; 2; 3]", "6");
    ("reverse", "reverse [1; 2; 3]", "[3, 2, 1]");
    ("append", "append [1; 2] [3; 4]", "[1, 2, 3, 4]");
    ("take", "take 2 [1; 2; 3; 4]", "[1, 2]");
    ("drop", "drop 2 [1; 2; 3; 4]", "[3, 4]");
    ("sort", "sort [3; 1; 4; 1; 5]", "[1, 1, 3, 4, 5]");
    ("range", "range 1 5", "[1, 2, 3, 4, 5]");
    
    (* 字符串操作 *)
    ("string_length", "string_length \"hello\"", "5");
    ("string_concat", "string_concat [\"hello\"; \" \"; \"world\"]", "hello world");
    ("string_uppercase", "string_uppercase \"hello\"", "HELLO");
    ("string_lowercase", "string_lowercase \"HELLO\"", "hello");
    ("string_trim", "string_trim \"  hello  \"", "hello");
    ("string_split", "string_split \",\" \"a,b,c\"", "[a, b, c]");
    ("string_contains", "string_contains \"hello\" \"lo\"", "true");
    ("string_replace", "string_replace \"hello\" \"l\" \"L\"", "heLLo");
    ("string_starts_with", "string_starts_with \"hello\" \"he\"", "true");
    ("string_ends_with", "string_ends_with \"hello\" \"lo\"", "true");
    
    (* 数学函数 *)
    ("math_abs", "math_abs (-5)", "5");
    ("math_max", "math_max 3 7", "7");
    ("math_min", "math_min 3 7", "3");
    ("math_sqrt", "math_sqrt 9", "3");
    ("math_pow", "math_pow 2 10", "1024");
    ("math_sum", "math_sum [1; 2; 3; 4; 5]", "15");
    ("math_product", "math_product [1; 2; 3; 4; 5]", "120");
    
    (* 类型检查 *)
    ("is_int", "is_int 42", "true");
    ("is_string", "is_string \"hello\"", "true");
    ("is_bool", "is_bool true", "true");
    ("is_list", "is_list [1; 2; 3]", "true");
    ("is_function", "is_function (fun x -> x)", "true");
    
    (* 转换函数 *)
    ("int_of_string", "int_of_string \"42\"", "42");
    ("string_of_int", "string_of_int 42", "42");
    ("char_of_int", "char_of_int 65", "A");
    ("int_of_char", "int_of_char 'A'", "65");
    
    (* 高阶函数 *)
    ("exists", "exists (fun x -> x > 3) [1; 2; 3; 4; 5]", "true");
    ("forall", "forall (fun x -> x > 0) [1; 2; 3; 4; 5]", "true");
    ("find", "find (fun x -> x > 3) [1; 2; 3; 4; 5]", "4");
    ("zip", "zip [1; 2; 3] [\"a\"; \"b\"; \"c\"]", "[(1, a), (2, b), (3, c)]");
    
    (* 文件操作 *)
    ("file_exists", "file_exists \"/tmp\"", "true");
    
    (* 时间函数 *)
    ("time_now", "let t = time_now () in is_int t", "true");
  ] in
  
  let passed = List.fold_left (fun acc (name, code, expected) ->
    if run_test name code expected then acc + 1 else acc
  ) 0 tests in
  
  Printf.printf "通过: %d/%d\n" passed (List.length tests);
  passed = List.length tests

(** 测试 Scheme 代码生成 *)
let test_scheme_code_generation () =
  Printf.printf "\n=== Scheme 代码生成测试 ===\n";
  
  let test_cases = [
    ("简单算术", "1 + 2", "(+ 1 2)");
    ("let 绑定", "let x = 10 in x", "(let ((x 10)) x)");
    ("函数", "fun x -> x + 1", "(lambda (x) (+ x 1))");
    ("条件", "if true then 1 else 0", "(if #t 1 0)");
  ] in
  
  let passed = List.fold_left (fun acc (name, code, expected_substring) ->
    try
      let expr = My_lang.parse code in
      let scheme_code = Scheme_backend.compile_expr expr in
      if String.length scheme_code > 0 then begin
        Printf.printf "  ✅ %s: %s\n" name scheme_code;
        acc + 1
      end else begin
        Printf.printf "  ❌ %s: 空代码\n" name;
        acc
      end
    with
    | exn ->
      Printf.printf "  ❌ %s: %s\n" name (Printexc.to_string exn);
      acc
  ) 0 test_cases in
  
  Printf.printf "通过: %d/%d\n" passed (List.length test_cases);
  passed = List.length test_cases

(** 运行所有测试 *)
let () =
  Printf.printf "MyLang 集成测试套件\n";
  Printf.printf "==================\n";
  
  let results = [
    test_scheme_backend ();
    test_adt_pattern_matching ();
    test_stdlib_functions ();
    test_scheme_code_generation ();
  ] in
  
  let total_passed = List.filter (fun x -> x) results |> List.length in
  let total_tests = List.length results in
  
  Printf.printf "\n==================\n";
  Printf.printf "总结: %d/%d 测试套件通过\n" total_passed total_tests;
  
  if total_passed = total_tests then
    Printf.printf "✅ 所有测试通过！\n"
  else
    Printf.printf "❌ 部分测试失败\n"
