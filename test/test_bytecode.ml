open Core
open My_lang

let test_bytecode name code =
  try
    let expr = My_lang.parse code in
    let bytecode = My_lang.compile expr in
    let result = My_lang.run_bytecode bytecode in
    printf "[PASS-BC] %s: %s\n" name (My_lang.Vm.string_of_vm_value result)
  with
  | exn -> printf "[FAIL-BC] %s: %s\n" name (Exn.to_string exn)

let () =
  test_bytecode "integer" "42";
  test_bytecode "arithmetic" "1 + 2 * 3";
  test_bytecode "let binding" "let x = 10 in x + 5";
  test_bytecode "boolean" "true && false";
  test_bytecode "if" "if 1 < 2 then 100 else 200";
  test_bytecode "function" "let f = fun x -> x + 1 in f 5";
  test_bytecode "string concat" "\"hello\" ^ \" \" ^ \"world\"";
  test_bytecode "list" "[1, 2, 3]";
  test_bytecode "tuple" "(1, 2)";
  test_bytecode "factorial" "let rec factorial = fun n -> if n = 0 then 1 else n * factorial (n - 1) in factorial 5";
  test_bytecode "match" "match 42 with | 0 -> 1 | 42 -> 2 | _ -> 3";
  printf "\nBytecode tests completed.\n"
