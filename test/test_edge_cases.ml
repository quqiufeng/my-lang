open Core
open My_lang

let test_division_by_zero () =
  match My_lang.run_exn "1 / 0" with
  | Ok _ -> printf "[FAIL] test_division_by_zero: expected error\n"
  | Error msg ->
      if String.is_substring msg ~substring:"除零" then
        printf "[PASS] test_division_by_zero\n"
      else
        printf "[FAIL] test_division_by_zero: expected division by zero error, got: %s\n" msg

let test_list_index_out_of_bounds () =
  match My_lang.run_exn "let x = [1; 2; 3] in x.[5]" with
  | Ok _ -> printf "[FAIL] test_list_index_out_of_bounds: expected error\n"
  | Error msg ->
      if String.is_substring msg ~substring:"越界" then
        printf "[PASS] test_list_index_out_of_bounds\n"
      else
        printf "[FAIL] test_list_index_out_of_bounds: expected out of bounds, got: %s\n" msg

let test_negative_index () =
  match My_lang.run_exn "let x = [1; 2; 3] in x.[-1]" with
  | Ok _ -> printf "[FAIL] test_negative_index: expected error\n"
  | Error msg ->
      if String.is_substring msg ~substring:"越界" then
        printf "[PASS] test_negative_index\n"
      else
        printf "[FAIL] test_negative_index: expected out of bounds, got: %s\n" msg

let test_string_index () =
  match My_lang.run_exn "\"hello\".[1]" with
  | Ok v ->
      if String.equal (Ast.string_of_value v) "\"e\"" then
        printf "[PASS] test_string_index\n"
      else
        printf "[FAIL] test_string_index: expected \"e\", got %s\n" (Ast.string_of_value v)
  | Error msg ->
      printf "[FAIL] test_string_index: unexpected error: %s\n" msg

let test_string_index_out_of_bounds () =
  match My_lang.run_exn "\"hello\".[10]" with
  | Ok _ -> printf "[FAIL] test_string_index_out_of_bounds: expected error\n"
  | Error msg ->
      if String.is_substring msg ~substring:"越界" then
        printf "[PASS] test_string_index_out_of_bounds\n"
      else
        printf "[FAIL] test_string_index_out_of_bounds: expected out of bounds, got: %s\n" msg

let test_gc_stress () =
  (* 创建大量短期对象触发 GC *)
  let code = "let rec loop = fun n -> if n = 0 then 0 else loop (n - 1) in loop 1000" in
  match My_lang.run_exn code with
  | Ok v ->
      if String.equal (Ast.string_of_value v) "0" then
        printf "[PASS] test_gc_stress\n"
      else
        printf "[FAIL] test_gc_stress: expected 0, got %s\n" (Ast.string_of_value v)
  | Error msg ->
      printf "[FAIL] test_gc_stress: unexpected error: %s\n" msg

let () =
  test_division_by_zero ();
  test_list_index_out_of_bounds ();
  test_negative_index ();
  test_string_index ();
  test_string_index_out_of_bounds ();
  test_gc_stress ();
  printf "\nEdge case tests completed.\n"
