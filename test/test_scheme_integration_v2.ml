(** Scheme 后端和标准库集成测试 v2

    修复了格式和调用方式问题。
*)

open My_lang

(** 测试结果类型 *)
type test_result = Pass | Fail of string | Error of exn

(** 测试辅助函数 *)
let run_test name code =
  Printf.printf "测试: %s\n" name;
  try
    let result = My_lang.run code in
    let result_str = Ast.string_of_value result in
    Printf.printf "  结果: %s\n" result_str;
    Pass
  with
  | exn -> Error exn

(** 测试 Scheme 后端编译 *)
let test_scheme_backend () =
  Printf.printf "\n=== Scheme 后端测试 ===\n";
  
  let tests = [
    ("算术运算", "1 + 2");
    ("let 绑定", "let x = 10 in x + 5");
    ("函数定义", "let f = fun x -> x * 2 in f 21");
    ("条件表达式", "if 1 > 0 then 1 else 0");
    ("递归函数", "let rec fib = fun n -> if n <= 1 then n else fib (n - 1) + fib (n - 2) in fib 10");
    ("列表字面量", "[1; 2; 3]");
    ("字符串字面量", "\"hello\"");
    ("布尔值", "true && false");
  ] in
  
  let results = List.map (fun (name, code) ->
    match run_test name code with
    | Pass -> true
    | Fail msg -> Printf.printf "  ❌ 失败: %s\n" msg; false
    | Error exn -> Printf.printf "  ❌ 异常: %s\n" (Printexc.to_string exn); false
  ) tests in
  
  let passed = List.filter (fun x -> x) results |> List.length in
  Printf.printf "通过: %d/%d\n" passed (List.length tests);
  passed = List.length tests

(** 测试 ADT 和模式匹配 *)
let test_adt_pattern_matching () =
  Printf.printf "\n=== ADT 和模式匹配测试 ===\n";
  
  let tests = [
    ("无参数 ADT", "type color = Red | Green | Blue; Red");
    ("有参数 ADT", "type option = Some of int | None; Some 42");
    ("模式匹配 - 匹配成功", "type option = Some of int | None; match Some 42 with | Some n -> n | None -> 0");
    ("模式匹配 - 匹配 None", "type option = Some of int | None; match None with | Some n -> n | None -> (-1)");
    ("嵌套 ADT", "type tree = Leaf of int | Node of int * int; Node(1, 2)");
  ] in
  
  let results = List.map (fun (name, code) ->
    match run_test name code with
    | Pass -> true
    | Fail msg -> Printf.printf "  ❌ 失败: %s\n" msg; false
    | Error exn -> Printf.printf "  ❌ 异常: %s\n" (Printexc.to_string exn); false
  ) tests in
  
  let passed = List.filter (fun x -> x) results |> List.length in
  Printf.printf "通过: %d/%d\n" passed (List.length tests);
  passed = List.length tests

(** 测试标准库函数 - 基础 *)
let test_stdlib_basic () =
  Printf.printf "\n=== 标准库基础函数测试 ===\n";
  
  let tests = [
    (* 基础函数 *)
    ("head", "head [1; 2; 3]");
    ("tail", "tail [1; 2; 3]");
    ("length", "length [1; 2; 3]");
    ("sum", "sum [1; 2; 3]");
    ("reverse", "reverse [1; 2; 3]");
    ("append", "append [1; 2] [3; 4]");
    
    (* 数学函数 *)
    ("abs", "abs (-5)");
    ("min", "min 3 7");
    ("max", "max 3 7");
    ("sqrt", "sqrt 9");
    ("pow", "pow 2 10");
    
    (* 类型检查 *)
    ("is_int", "is_int 42");
    ("is_string", "is_string \"hello\"");
    ("is_bool", "is_bool true");
    ("is_list", "is_list [1; 2; 3]");
    ("is_function", "is_function (fun x -> x)");
    
    (* 转换函数 *)
    ("int_of_string", "int_of_string \"42\"");
    ("string_of_int", "string_of_int 42");
  ] in
  
  let results = List.map (fun (name, code) ->
    match run_test name code with
    | Pass -> true
    | Fail msg -> Printf.printf "  ❌ 失败: %s\n" msg; false
    | Error exn -> Printf.printf "  ❌ 异常: %s\n" (Printexc.to_string exn); false
  ) tests in
  
  let passed = List.filter (fun x -> x) results |> List.length in
  Printf.printf "通过: %d/%d\n" passed (List.length tests);
  passed = List.length tests

(** 测试标准库函数 - 字符串 *)
let test_stdlib_string () =
  Printf.printf "\n=== 标准库字符串函数测试 ===\n";
  
  let tests = [
    ("string_length", "string_length \"hello\"");
    ("string_uppercase", "string_uppercase \"hello\"");
    ("string_lowercase", "string_lowercase \"HELLO\"");
    ("string_trim", "string_trim \"  hello  \"");
    ("string_contains", "string_contains \"hello\" \"lo\"");
    ("string_starts_with", "string_starts_with \"hello\" \"he\"");
    ("string_ends_with", "string_ends_with \"hello\" \"lo\"");
  ] in
  
  let results = List.map (fun (name, code) ->
    match run_test name code with
    | Pass -> true
    | Fail msg -> Printf.printf "  ❌ 失败: %s\n" msg; false
    | Error exn -> Printf.printf "  ❌ 异常: %s\n" (Printexc.to_string exn); false
  ) tests in
  
  let passed = List.filter (fun x -> x) results |> List.length in
  Printf.printf "通过: %d/%d\n" passed (List.length tests);
  passed = List.length tests

(** 测试 Scheme 代码生成 *)
let test_scheme_code_generation () =
  Printf.printf "\n=== Scheme 代码生成测试 ===\n";
  
  let test_cases = [
    ("简单算术", "1 + 2");
    ("let 绑定", "let x = 10 in x");
    ("函数", "fun x -> x + 1");
    ("条件", "if true then 1 else 0");
    ("递归", "let rec f = fun n -> if n <= 0 then 0 else n + f (n - 1) in f 5");
  ] in
  
  let results = List.map (fun (name, code) ->
    try
      let expr = My_lang.parse code in
      let scheme_code = Scheme_backend.compile_expr expr in
      if String.length scheme_code > 0 then begin
        Printf.printf "  ✅ %s: %s\n" name scheme_code;
        true
      end else begin
        Printf.printf "  ❌ %s: 空代码\n" name;
        false
      end
    with
    | exn ->
      Printf.printf "  ❌ %s: %s\n" name (Printexc.to_string exn);
      false
  ) test_cases in
  
  let passed = List.filter (fun x -> x) results |> List.length in
  Printf.printf "通过: %d/%d\n" passed (List.length test_cases);
  passed = List.length test_cases

(** 测试 AoT 编译 *)
let test_aot_compilation () =
  Printf.printf "\n=== AoT 编译测试 ===\n";
  
  let temp_file = Filename.temp_file "mylang_test" ".ml" in
  let output_file = Filename.temp_file "mylang_test" "" in
  
  (* 写入测试代码 *)
  let oc = open_out temp_file in
  output_string oc "let rec fib = fun n -> if n <= 1 then n else fib (n - 1) + fib (n - 2) in fib 10";
  close_out oc;
  
  (* 测试 AoT 编译 *)
  match Aot.compile_standalone temp_file output_file with
  | Ok msg ->
    Printf.printf "  ✅ AoT 编译成功: %s\n" msg;
    
    (* 测试执行 *)
    let cmd = Printf.sprintf "%s 2>/dev/null" output_file in
    let result = Sys.command cmd in
    if result = 0 then begin
      Printf.printf "  ✅ 执行成功\n";
      true
    end else begin
      Printf.printf "  ❌ 执行失败: 退出码 %d\n" result;
      false
    end
  | Error err ->
    Printf.printf "  ❌ AoT 编译失败: %s\n" err;
    false

(** 运行所有测试 *)
let () =
  Printf.printf "MyLang 集成测试套件 v2\n";
  Printf.printf "======================\n";
  
  let results = [
    (test_scheme_backend (), "Scheme后端");
    (test_adt_pattern_matching (), "ADT和模式匹配");
    (test_stdlib_basic (), "标准库基础");
    (test_stdlib_string (), "标准库字符串");
    (test_scheme_code_generation (), "Scheme代码生成");
  ] in
  
  let aot_result = test_aot_compilation () in
  
  let total_passed = List.fold_left (fun acc (r, _) -> if r then acc + 1 else acc) 0 results in
  let total_tests = List.length results in
  
  Printf.printf "\n======================\n";
  Printf.printf "总结: %d/%d 测试套件通过\n" total_passed total_tests;
  
  if total_passed = total_tests && aot_result then
    Printf.printf "✅ 所有测试通过！\n"
  else
    Printf.printf "❌ 部分测试失败\n"
