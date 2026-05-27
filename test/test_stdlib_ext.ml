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
  Printf.printf "=== Standard Library Tests ===\n\n";
  
  (* ===== HashMap 测试 ===== *)
  Printf.printf "-- HashMap --\n";
  test "hashmap_create" "hashmap_create ()" "{}";
  test "hashmap_set" "hashmap_set (hashmap_create (), \"x\", 42)" "{x = 42}";
  test "hashmap_get_some" "hashmap_get (hashmap_set (hashmap_create (), \"x\", 42), \"x\")" "Some 42";
  test "hashmap_get_none" "hashmap_get (hashmap_create (), \"x\")" "None";
  test "hashmap_delete" "hashmap_size (hashmap_delete (hashmap_set (hashmap_create (), \"x\", 42), \"x\"))" "0";
  test "hashmap_keys" "hashmap_keys (hashmap_set (hashmap_set (hashmap_create (), \"x\", 1), \"y\", 2))" "[\"y\"; \"x\"]";
  test "hashmap_values" "hashmap_values (hashmap_set (hashmap_set (hashmap_create (), \"x\", 1), \"y\", 2))" "[2; 1]";
  test "hashmap_size" "hashmap_size (hashmap_create ())" "0";
  test "hashmap_size_1" "hashmap_size (hashmap_set (hashmap_create (), \"x\", 1))" "1";
  test "hashmap_has_key_true" "hashmap_has_key (hashmap_set (hashmap_create (), \"x\", 1), \"x\")" "true";
  test "hashmap_has_key_false" "hashmap_has_key (hashmap_create (), \"x\")" "false";
  
  (* ===== IO 测试 ===== *)
  Printf.printf "\n-- IO --\n";
  test "read_lines" "length (read_lines \"/etc/hostname\") > 0" "true";
  test_error "read_lines_error" "read_lines \"/nonexistent\"";
  
  (* ===== 字符串增强测试 ===== *)
  Printf.printf "\n-- String Enhanced --\n";
  test "string_starts_with_true" "string_starts_with (\"hello\", \"he\")" "true";
  test "string_starts_with_false" "string_starts_with (\"hello\", \"wo\")" "false";
  test "string_ends_with_true" "string_ends_with (\"hello\", \"lo\")" "true";
  test "string_ends_with_false" "string_ends_with (\"hello\", \"he\")" "false";
  test "string_repeat" "string_repeat (\"ab\", 3)" "\"ababab\"";
  test "string_repeat_0" "string_repeat (\"ab\", 0)" "\"\"";
  test "string_pad_left" "string_pad_left (\"42\", 5, \"0\")" "\"00042\"";
  test "string_pad_right" "string_pad_right (\"42\", 5, \"0\")" "\"42000\"";
  
  (* ===== 列表增强测试 ===== *)
  Printf.printf "\n-- List Enhanced --\n";
  test "list_flatten" "list_flatten [[1, 2], [3, 4], [5]]" "[1; 2; 3; 4; 5]";
  test "list_flatten_empty" "list_flatten []" "[]";
  test "list_distinct" "list_distinct [1, 2, 2, 3, 3, 3]" "[1; 2; 3]";
  test "list_distinct_empty" "list_distinct []" "[]";
  
  (* ===== 数学测试 ===== *)
  Printf.printf "\n-- Math --\n";
  test "math_abs_positive" "math_abs 5" "5";
  test "math_abs_negative" "math_abs (-5)" "5";
  test "math_abs_zero" "math_abs 0" "0";
  test "math_min" "math_min (3, 5)" "3";
  test "math_max" "math_max (3, 5)" "5";
  test "math_clamp" "math_clamp (10, 0, 5)" "5";
  test "math_clamp_low" "math_clamp (-5, 0, 5)" "0";
  test "math_clamp_ok" "math_clamp (3, 0, 5)" "3";
  test "math_sum" "math_sum [1, 2, 3, 4, 5]" "15";
  test "math_sum_empty" "math_sum []" "0";
  test "math_product" "math_product [2, 3, 4]" "24";
  test "math_product_empty" "math_product []" "1";
  
  (* ===== 转换测试 ===== *)
  Printf.printf "\n-- Conversion --\n";
  test "int_to_string" "int_to_string 42" "\"42\"";
  test "int_to_string_negative" "int_to_string (-5)" "\"-5\"";
  test "string_to_int" "string_to_int \"42\"" "42";
  test_error "string_to_int_error" "string_to_int \"abc\"";
  test "bool_to_string_true" "bool_to_string true" "\"true\"";
  test "bool_to_string_false" "bool_to_string false" "\"false\"";
  test "char_to_string" "char_to_string 'a'" "\"a\"";
  
  (* ===== JSON 测试 ===== *)
  Printf.printf "\n-- JSON --\n";
  test "json_parse_null" "json_parse \"null\"" "()";
  test "json_parse_bool" "json_parse \"true\"" "true";
  test "json_parse_int" "json_parse \"42\"" "42";
  test "json_parse_string" "json_parse \"\\\"hello\\\"\"" "\"hello\"";
  test "json_parse_array" "json_parse \"[1, 2, 3]\"" "[1; 2; 3]";
  test "json_parse_object" "json_parse \"{\\\"x\\\": 1, \\\"y\\\": 2}\"" "{x = 1; y = 2}";
  test_error "json_parse_error" "json_parse \"invalid\"";
  test "json_stringify_null" "json_stringify ()" "\"[]\"";
  test "json_stringify_bool" "json_stringify true" "\"true\"";
  test "json_stringify_int" "json_stringify 42" "\"42\"";
  test "json_stringify_string" "json_stringify \"hello\"" "\"\"hello\"\"";
  test "json_stringify_array" "json_stringify [1, 2, 3]" "\"[1,2,3]\"";
  
  (* ===== DateTime 测试 ===== *)
  Printf.printf "\n-- DateTime --\n";
  test "time_now" "time_now () > 0" "true";
  test "time_now_ms" "time_now_ms () > 0" "true";
  test "time_year" "time_year (time_now ()) >= 2024" "true";
  test "time_format" "time_format (1704067200, \"%Y-%m-%d %H:%M:%S\")" "\"2024-01-01 08:00:00\"";
  
  (* ===== Set 测试 ===== *)
  Printf.printf "\n-- Set --\n";
  test "set_create" "set_create ()" "[]";
  test "set_add" "set_add (set_create (), 1)" "[1]";
  test "set_add_duplicate" "set_add (set_add (set_create (), 1), 1)" "[1]";
  test "set_remove" "set_remove (set_add (set_create (), 1), 1)" "[]";
  test "set_contains_true" "set_contains (set_add (set_create (), 1), 1)" "true";
  test "set_contains_false" "set_contains (set_create (), 1)" "false";
  test "set_size" "set_size (set_add (set_add (set_create (), 1), 2))" "2";
  test "set_union" "set_union (set_add (set_create (), 1), set_add (set_create (), 2))" "[1; 2]";
  test "set_intersection" "set_intersection (set_add (set_add (set_create (), 1), 2), set_add (set_add (set_create (), 2), 3))" "[2]";
  test "set_difference" "set_difference (set_add (set_add (set_create (), 1), 2), set_add (set_create (), 2))" "[1]";
  
  Printf.printf "\n=== Results: %d/%d passed ===\n" !pass_count !test_count
