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
  test_bytecode "match int" "match 42 with | 0 -> 1 | 42 -> 2 | _ -> 3";
  test_bytecode "match bool" "match true with | true -> 1 | false -> 0";
  test_bytecode "match string" "match \"hello\" with | \"world\" -> 1 | \"hello\" -> 2 | _ -> 3";
  test_bytecode "match var" "match 42 with | x -> x + 1";
  test_bytecode "match cons" "match [1, 2, 3] with | [] -> 0 | h :: t -> h + length t";
  test_bytecode "while" "while false do 42 done";
  test_bytecode "list index" "[10, 20, 30][1]";
  test_bytecode "string index" "\"hello\"[1]";
  test_bytecode "list slice" "[1, 2, 3, 4, 5][1:3]";
  test_bytecode "string slice" "\"hello\"[1:4]";
  test_bytecode "tuple pattern" "match (1, 2, 3) with | (a, b, c) -> a + b + c";
  test_bytecode "list pattern" "match [10, 20, 30] with | [a, b, c] -> a + b + c";
  test_bytecode "nested list const" "match [1, 2, 3] with | [1, x, 3] -> x + 10 | _ -> 0";
  test_bytecode "nested tuple const" "match (1, 2, 3) with | (1, x, 3) -> x + 10 | _ -> 0";
  test_bytecode "nested cons" "match [1, 2, 3] with | 1 :: t -> length t | _ -> 0";
  test_bytecode "try catch" "try raise 42 with | x -> x + 1";
  test_bytecode "try no raise" "try 100 with | x -> x + 1";
  test_bytecode "try match" "try raise 42 with | 0 -> 0 | x -> x * 2";
  test_bytecode "assert true" "assert true";
  test_bytecode "ignore" "ignore 42";
  test_bytecode "pipe" "5 |> (fun x -> x + 1)";
  test_bytecode "pipe chain" "5 |> (fun x -> x + 1) |> (fun x -> x * 2)";
  test_bytecode "type annot" "let x : int = 42 in x";
  test_bytecode "type annot bool" "let x : bool = true in if x then 1 else 0";
  test_bytecode "record pattern" "match {name = \"x\"; age = 1} with | {name = n; age = a} -> a + 1";
  test_bytecode "record pattern shorthand" "match {name = \"x\"; age = 1} with | {name; age} -> age + 1";
  printf "\nBytecode tests completed.\n"
