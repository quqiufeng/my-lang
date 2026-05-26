open Core
open My_lang

let test_diagnostics_create () =
  let diag = Diagnostics.create () in
  if Diagnostics.has_errors diag then
    printf "[FAIL] test_diagnostics_create: should not have errors initially\n"
  else if Diagnostics.error_count diag <> 0 then
    printf "[FAIL] test_diagnostics_create: should have 0 errors\n"
  else if Diagnostics.warning_count diag <> 0 then
    printf "[FAIL] test_diagnostics_create: should have 0 warnings\n"
  else
    printf "[PASS] test_diagnostics_create\n"

let test_add_error () =
  let diag = Diagnostics.create () in
  Diagnostics.add_error diag ~line:1 ~col:5 "类型错误";
  if not (Diagnostics.has_errors diag) then
    printf "[FAIL] test_add_error: should have errors\n"
  else if Diagnostics.error_count diag <> 1 then
    printf "[FAIL] test_add_error: should have 1 error\n"
  else
    printf "[PASS] test_add_error\n"

let test_add_warning () =
  let diag = Diagnostics.create () in
  Diagnostics.add_warning diag ~line:2 ~col:3 "未使用的变量";
  if Diagnostics.has_errors diag then
    printf "[FAIL] test_add_warning: should not have errors\n"
  else if Diagnostics.warning_count diag <> 1 then
    printf "[FAIL] test_add_warning: should have 1 warning\n"
  else
    printf "[PASS] test_add_warning\n"

let test_multiple_diagnostics () =
  let diag = Diagnostics.create () in
  Diagnostics.add_error diag ~line:1 ~col:1 "错误1";
  Diagnostics.add_error diag ~line:2 ~col:2 "错误2";
  Diagnostics.add_warning diag ~line:3 ~col:3 "警告1";
  if Diagnostics.error_count diag <> 2 then
    printf "[FAIL] test_multiple_diagnostics: expected 2 errors, got %d\n" (Diagnostics.error_count diag)
  else if Diagnostics.warning_count diag <> 1 then
    printf "[FAIL] test_multiple_diagnostics: expected 1 warning, got %d\n" (Diagnostics.warning_count diag)
  else
    printf "[PASS] test_multiple_diagnostics\n"

let test_format_summary () =
  let diag = Diagnostics.create () in
  let summary = Diagnostics.format_summary diag in
  if not (String.equal summary "编译成功，没有诊断信息") then
    printf "[FAIL] test_format_summary: expected empty summary, got '%s'\n" summary
  else
    let () = Diagnostics.add_error diag "错误1" in
    let summary = Diagnostics.format_summary diag in
    if not (String.is_substring summary ~substring:"1 个错误") then
      printf "[FAIL] test_format_summary: expected '1 个错误', got '%s'\n" summary
    else
      printf "[PASS] test_format_summary\n"

let test_format_all () =
  let diag = Diagnostics.create () in
  Diagnostics.add_error diag ~line:1 ~col:5 ~highlight_len:3 "测试错误";
  let formatted = Diagnostics.format_all diag in
  if not (String.is_substring formatted ~substring:"测试错误") then
    printf "[FAIL] test_format_all: expected '测试错误' in output\n"
  else
    printf "[PASS] test_format_all\n"

let test_phase_counts () =
  let diag = Diagnostics.create () in
  Diagnostics.add_error diag ~phase:Diagnostics.Parsing "解析错误";
  Diagnostics.add_error diag ~phase:Diagnostics.Parsing "另一个解析错误";
  Diagnostics.add_error diag ~phase:Diagnostics.TypeChecking "类型错误";
  if Diagnostics.error_count diag <> 3 then
    printf "[FAIL] test_phase_counts: expected 3 errors, got %d\n" (Diagnostics.error_count diag)
  else
    printf "[PASS] test_phase_counts\n"

let () =
  test_diagnostics_create ();
  test_add_error ();
  test_add_warning ();
  test_multiple_diagnostics ();
  test_format_summary ();
  test_format_all ();
  test_phase_counts ();
  printf "\nDiagnostics tests completed.\n"
