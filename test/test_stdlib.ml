(** 标准库测试 *)

open Core
open My_lang
open My_lang.Ast

let run_test name code check =
  match My_lang.run_exn code with
  | Ok v when check v ->
      printf "[PASS] %s\n" name
  | Ok v ->
      printf "[FAIL] %s: got %s\n" name (string_of_value v)
  | Error msg ->
      printf "[FAIL] %s: unexpected error: %s\n" name msg

let () =
  run_test "string_trim"
    "string_trim(\"  hello  \")"
    (function VString "hello" -> true | _ -> false);

  run_test "string_uppercase"
    "string_uppercase(\"hello\")"
    (function VString "HELLO" -> true | _ -> false);

  run_test "string_lowercase"
    "string_lowercase(\"HELLO\")"
    (function VString "hello" -> true | _ -> false);

  run_test "string_concat"
    "string_concat(\", \", [\"a\", \"b\", \"c\"])"
    (function VString "a, b, c" -> true | _ -> false);

  run_test "string_split"
    "string_split(\",\", \"a,b,c\")"
    (function VList [VString "a"; VString "b"; VString "c"] -> true | _ -> false);

  run_test "string_contains_true"
    "string_contains(\"ell\", \"hello\")"
    (function VBool true -> true | _ -> false);

  run_test "string_contains_false"
    "string_contains(\"xyz\", \"hello\")"
    (function VBool false -> true | _ -> false);

  run_test "string_replace"
    "string_replace(\"ell\", \"ELL\", \"hello\")"
    (function VString "hELLo" -> true | _ -> false);

  run_test "take"
    "take(2, [1, 2, 3, 4])"
    (function VList [VInt 1; VInt 2] -> true | _ -> false);

  run_test "drop"
    "drop(2, [1, 2, 3, 4])"
    (function VList [VInt 3; VInt 4] -> true | _ -> false);

  run_test "find_some"
    "find((fun x -> x > 2), [1, 2, 3, 4])"
    (function VCtor ("Some", Some (VInt 3)) -> true | _ -> false);

  run_test "find_none"
    "find((fun x -> x > 10), [1, 2, 3, 4])"
    (function VCtor ("None", None) -> true | _ -> false);

  run_test "exists_true"
    "exists((fun x -> x > 2), [1, 2, 3, 4])"
    (function VBool true -> true | _ -> false);

  run_test "exists_false"
    "exists((fun x -> x > 10), [1, 2, 3, 4])"
    (function VBool false -> true | _ -> false);

  run_test "forall_true"
    "forall((fun x -> x > 0), [1, 2, 3, 4])"
    (function VBool true -> true | _ -> false);

  run_test "forall_false"
    "forall((fun x -> x > 2), [1, 2, 3, 4])"
    (function VBool false -> true | _ -> false);

  run_test "sort_int"
    "sort([3, 1, 4, 1, 5])"
    (function VList [VInt 1; VInt 1; VInt 3; VInt 4; VInt 5] -> true | _ -> false);

  run_test "zip"
    "zip([1, 2], [\"a\", \"b\"])"
    (function VList [VTuple [VInt 1; VString "a"]; VTuple [VInt 2; VString "b"]] -> true | _ -> false);

  run_test "abs"
    "abs(-5)"
    (function VInt 5 -> true | _ -> false);

  run_test "min"
    "min(3, 5)"
    (function VInt 3 -> true | _ -> false);

  run_test "max"
    "max(3, 5)"
    (function VInt 5 -> true | _ -> false);

  run_test "int_of_string"
    "int_of_string(\"42\")"
    (function VInt 42 -> true | _ -> false);

  run_test "string_of_int"
    "string_of_int(42)"
    (function VString "42" -> true | _ -> false);

  run_test "int_of_char"
    "(int_of_char 'A')"
    (function VInt 65 -> true | _ -> false);

  run_test "char_of_int"
    "char_of_int(65)"
    (function VChar 'A' -> true | _ -> false);

  printf "Standard library tests done.\n"
