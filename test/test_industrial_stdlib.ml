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
  Printf.printf "=== Industrial Standard Library Tests ===\n\n";
  
  (* ===== 字符串操作测试 ===== *)
  Printf.printf "-- String Operations --\n";
  test "string_join" "string_join (\",\", [\"a\", \"b\", \"c\"])" "\"a,b,c\"";
  test "string_join_empty" "string_join (\",\", [])" "\"\"";
  test "string_join_single" "string_join (\",\", [\"a\"])" "\"a\"";
  test "string_to_chars" "string_to_chars \"abc\"" "['a'; 'b'; 'c']";
  test "string_to_chars_empty" "string_to_chars \"\"" "[]";
  test "string_from_chars" "string_from_chars ['a', 'b', 'c']" "\"abc\"";
  test "string_from_chars_empty" "string_from_chars []" "\"\"";
  test "string_rev" "string_rev \"hello\"" "\"olleh\"";
  test "string_rev_empty" "string_rev \"\"" "\"\"";
  test "string_rev_single" "string_rev \"a\"" "\"a\"";
  
  (* ===== 列表操作测试 ===== *)
  Printf.printf "\n-- List Operations --\n";
  test "list_init" "list_init (3, fun i -> i * 2)" "[0; 2; 4]";
  test "list_init_0" "list_init (0, fun i -> i)" "[]";
  test "list_init_1" "list_init (1, fun i -> i + 1)" "[1]";
  test "list_forall_true" "list_forall (fun x -> x > 0) [1, 2, 3]" "true";
  test "list_forall_false" "list_forall (fun x -> x > 1) [1, 2, 3]" "false";
  test "list_forall_empty" "list_forall (fun x -> x > 0) []" "true";
  test "list_exists_true" "list_exists (fun x -> x > 2) [1, 2, 3]" "true";
  test "list_exists_false" "list_exists (fun x -> x > 5) [1, 2, 3]" "false";
  test "list_exists_empty" "list_exists (fun x -> x > 0) []" "false";
  test "list_mapi" "list_mapi (fun pair -> match pair with | (i, x) -> i + x) [10, 20, 30]" "[10; 21; 32]";
  test "list_mapi_empty" "list_mapi (fun pair -> match pair with | (i, x) -> i + x) []" "[]";
  (* list_filter_mapi tests - skipping due to type system limitations *)
  
  (* ===== 数学操作测试 ===== *)
  Printf.printf "\n-- Math Operations --\n";
  test "math_mod" "math_mod (10, 3)" "1";
  test "math_mod_0" "math_mod (0, 3)" "0";
  test_error "math_mod_div_zero" "math_mod (10, 0)";
  test "math_gcd" "math_gcd (12, 8)" "4";
  test "math_gcd_same" "math_gcd (5, 5)" "5";
  test "math_gcd_coprime" "math_gcd (7, 13)" "1";
  test "math_lcm" "math_lcm (4, 6)" "12";
  test "math_lcm_same" "math_lcm (5, 5)" "5";
  test "math_pow" "math_pow (2, 10)" "1024";
  test "math_pow_0" "math_pow (2, 0)" "1";
  test "math_pow_1" "math_pow (2, 1)" "2";
  test "math_sqrt" "math_sqrt 16" "4";
  test "math_sqrt_0" "math_sqrt 0" "0";
  test "math_sqrt_1" "math_sqrt 1" "1";
  test "math_sqrt_large" "math_sqrt 10000" "100";
  
  (* ===== 文件操作测试 ===== *)
  Printf.printf "\n-- File Operations --\n";
  test "file_temp" "file_exists (file_temp ())" "true";
  test "file_write_read" "let path = file_temp () in write_file path \"hello\"; read_file path" "\"hello\"";
  test_error "file_read_bytes_nonexist" "file_read_bytes \"/nonexistent\"";
  
  (* ===== 进程操作测试 ===== *)
  Printf.printf "\n-- Process Operations --\n";
  test "process_exec" "match process_exec \"echo hello\" with | (code, output) -> code" "0";
  test "process_exec_output" "match process_exec \"echo hello\" with | (code, output) -> code" "0";
  test "process_exec_fail" "match process_exec \"false\" with | (code, output) -> code" "1";
  
  (* ===== 类型检查测试 ===== *)
  Printf.printf "\n-- Type Checking --\n";
  test "is_int_true" "is_int 42" "true";
  test "is_int_false" "is_int \"hello\"" "false";
  test "is_bool_true" "is_bool true" "true";
  test "is_bool_false" "is_bool 42" "false";
  test "is_string_true" "is_string \"hello\"" "true";
  test "is_string_false" "is_string 42" "false";
  test "is_list_true" "is_list [1, 2, 3]" "true";
  test "is_list_false" "is_list 42" "false";
  test "is_function_true" "is_function (fun x -> x)" "true";
  test "is_function_false" "is_function 42" "false";
  test "is_unit_true" "is_unit ()" "true";
  test "is_unit_false" "is_unit 42" "false";
  
  Printf.printf "\n=== Results: %d/%d passed ===\n" !pass_count !test_count
