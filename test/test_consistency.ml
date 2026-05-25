open Core
open My_lang

let test_cases = [
  "42";
  "1 + 2 * 3";
  "let x = 10 in x + 5";
  "true && false";
  "if 1 < 2 then 100 else 200";
  "let f = fun x -> x + 1 in f 5";
  "\"hello\" ^ \" \" ^ \"world\"";
  "[1, 2, 3]";
  "(1, 2)";
  "let rec factorial = fun n -> if n = 0 then 1 else n * factorial (n - 1) in factorial 5";
  "match 42 with | 0 -> 1 | 42 -> 2 | _ -> 3";
  "match true with | true -> 1 | false -> 0";
  "match \"hello\" with | \"world\" -> 1 | \"hello\" -> 2 | _ -> 3";
  "match 42 with | x -> x + 1";
  "match [1, 2, 3] with | [] -> 0 | h :: t -> h + length t";
  "while false do 42 done";
  "[10, 20, 30][1]";
  "\"hello\"[1]";
  "[1, 2, 3, 4, 5][1:3]";
  "\"hello\"[1:4]";
  "match (1, 2, 3) with | (a, b, c) -> a + b + c";
  "match [10, 20, 30] with | [a, b, c] -> a + b + c";
  "match [1, 2, 3] with | [1, x, 3] -> x + 10 | _ -> 0";
  "match (1, 2, 3) with | (1, x, 3) -> x + 10 | _ -> 0";
  "match [1, 2, 3] with | 1 :: t -> length t | _ -> 0";
  "try raise 42 with | x -> x + 1";
  "try 100 with | x -> x + 1";
  "try raise 42 with | 0 -> 0 | x -> x * 2";
]

let () =
  let failed = ref 0 in
  let passed = ref 0 in
  List.iter test_cases ~f:(fun code ->
    try
      let expr = My_lang.parse code in
      let interp_result = My_lang.eval expr in
      let bytecode = My_lang.compile expr in
      let vm_result = My_lang.run_bytecode bytecode in
      let interp_str = My_lang.Ast.string_of_value interp_result in
      let vm_str = My_lang.Vm.string_of_vm_value vm_result in
      if String.equal interp_str vm_str then (
        printf "[PASS] %s\n" code;
        incr passed
      ) else (
        printf "[FAIL] %s\n" code;
        printf "  interp: %s\n" interp_str;
        printf "  bytecode: %s\n" vm_str;
        incr failed
      )
    with
    | exn ->
        printf "[ERROR] %s: %s\n" code (Exn.to_string exn);
        incr failed
  );
  printf "\n%d passed, %d failed\n" !passed !failed;
  if !failed > 0 then exit 1
