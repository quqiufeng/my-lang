open Core
open My_lang

let test_sqrt () =
  match My_lang.run_exn "sqrt 16" with
  | Ok v ->
      if String.equal (Ast.string_of_value v) "4" then
        printf "[PASS] test_sqrt\n"
      else
        printf "[FAIL] test_sqrt: expected 4, got %s\n" (Ast.string_of_value v)
  | Error msg -> printf "[FAIL] test_sqrt: %s\n" msg

let test_pow () =
  match My_lang.run_exn "pow (2, 10)" with
  | Ok v ->
      if String.equal (Ast.string_of_value v) "1024" then
        printf "[PASS] test_pow\n"
      else
        printf "[FAIL] test_pow: expected 1024, got %s\n" (Ast.string_of_value v)
  | Error msg -> printf "[FAIL] test_pow: %s\n" msg

let test_random_int () =
  match My_lang.run_exn "random_int (1, 10)" with
  | Ok v ->
      printf "[PASS] test_random_int: got %s\n" (Ast.string_of_value v)
  | Error msg -> printf "[FAIL] test_random_int: %s\n" msg

let test_current_time () =
  match My_lang.run_exn "current_time ()" with
  | Ok v ->
      printf "[PASS] test_current_time: got %s\n" (Ast.string_of_value v)
  | Error msg -> printf "[FAIL] test_current_time: %s\n" msg

let test_file_exists () =
  match My_lang.run_exn "file_exists \"/etc/passwd\"" with
  | Ok v ->
      if String.equal (Ast.string_of_value v) "true" then
        printf "[PASS] test_file_exists\n"
      else
        printf "[FAIL] test_file_exists: expected true\n"
  | Error msg -> printf "[FAIL] test_file_exists: %s\n" msg

let test_get_env () =
  match My_lang.run_exn "get_env \"HOME\"" with
  | Ok v ->
      printf "[PASS] test_get_env: got %s\n" (Ast.string_of_value v)
  | Error msg -> printf "[FAIL] test_get_env: %s\n" msg

let test_system_command () =
  match My_lang.run_exn "system_command \"echo hello\"" with
  | Ok v ->
      printf "[PASS] test_system_command: got %s\n" (Ast.string_of_value v)
  | Error msg -> printf "[FAIL] test_system_command: %s\n" msg

let test_read_write_file () =
  let tmp = "/tmp/my_lang_test_rw.txt" in
  let write_code = Printf.sprintf "write_file \"%s\" \"hello world\"" tmp in
  match My_lang.run_exn write_code with
  | Ok _ ->
      let read_code = Printf.sprintf "read_file \"%s\"" tmp in
      (match My_lang.run_exn read_code with
       | Ok v ->
           if String.equal (Ast.string_of_value v) "\"hello world\"" then
             printf "[PASS] test_read_write_file\n"
           else
             printf "[FAIL] test_read_write_file: expected \"hello world\", got %s\n" (Ast.string_of_value v)
       | Error msg -> printf "[FAIL] test_read_write_file read: %s\n" msg)
  | Error msg -> printf "[FAIL] test_read_write_file write: %s\n" msg

let test_regex_match () =
  match My_lang.run_exn "regex_match (\"a+b\", \"aaab\")" with
  | Ok v ->
      if String.equal (Ast.string_of_value v) "true" then
        printf "[PASS] test_regex_match\n"
      else
        printf "[FAIL] test_regex_match: expected true, got %s\n" (Ast.string_of_value v)
  | Error msg -> printf "[FAIL] test_regex_match: %s\n" msg

let test_regex_replace () =
  match My_lang.run_exn "regex_replace (\"world\", \"my-lang\", \"hello world\")" with
  | Ok v ->
      if String.equal (Ast.string_of_value v) "\"hello my-lang\"" then
        printf "[PASS] test_regex_replace\n"
      else
        printf "[FAIL] test_regex_replace: expected \"hello my-lang\", got %s\n" (Ast.string_of_value v)
  | Error msg -> printf "[FAIL] test_regex_replace: %s\n" msg

let test_regex_split () =
  match My_lang.run_exn "regex_split (\",\", \"a,b,c\")" with
  | Ok v ->
      let expected = "[\"a\"; \"b\"; \"c\"]" in
      if String.equal (Ast.string_of_value v) expected then
        printf "[PASS] test_regex_split\n"
      else
        printf "[FAIL] test_regex_split: expected %s, got %s\n" expected (Ast.string_of_value v)
  | Error msg -> printf "[FAIL] test_regex_split: %s\n" msg

let () =
  test_sqrt ();
  test_pow ();
  test_random_int ();
  test_current_time ();
  test_file_exists ();
  test_get_env ();
  test_system_command ();
  test_read_write_file ();
  test_regex_match ();
  test_regex_replace ();
  test_regex_split ();
  printf "\nStandard library extension tests completed.\n"
