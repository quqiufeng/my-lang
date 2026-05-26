(** 寄存器 VM 测试 *)

open Core
open My_lang

let run_reg_vm code =
  let expr = My_lang.parse code in
  let prog = Reg_compiler.compile_program [expr] in
  Reg_vm.execute prog

let test name code expected =
  try
    let result = run_reg_vm code in
    match result, expected with
    | Reg_bytecode.RVInt a, Reg_bytecode.RVInt b when a = b ->
        Printf.printf "[PASS] %s\n" name
    | Reg_bytecode.RVBool a, Reg_bytecode.RVBool b when Bool.equal a b ->
        Printf.printf "[PASS] %s\n" name
    | Reg_bytecode.RVString a, Reg_bytecode.RVString b when String.equal a b ->
        Printf.printf "[PASS] %s\n" name
    | Reg_bytecode.RVList a, Reg_bytecode.RVList b
    | Reg_bytecode.RVTuple a, Reg_bytecode.RVTuple b ->
        if List.equal (fun x y -> String.equal (Reg_bytecode.string_of_reg_value x) (Reg_bytecode.string_of_reg_value y)) a b then
          Printf.printf "[PASS] %s\n" name
        else
          Printf.printf "[FAIL] %s: got %s, expected %s\n" name
            (Reg_bytecode.string_of_reg_value result)
            (Reg_bytecode.string_of_reg_value expected)
    | _ ->
        Printf.printf "[FAIL] %s: got %s, expected %s\n" name
          (Reg_bytecode.string_of_reg_value result)
          (Reg_bytecode.string_of_reg_value expected)
  with exn ->
    Printf.printf "[FAIL] %s: exception %s\n" name (Exn.to_string exn)

let () =
  test "addition" "1 + 2" (Reg_bytecode.RVInt 3);
  test "subtraction" "5 - 3" (Reg_bytecode.RVInt 2);
  test "multiplication" "2 * 3" (Reg_bytecode.RVInt 6);
  test "division" "6 / 2" (Reg_bytecode.RVInt 3);
  test "equality" "1 = 1" (Reg_bytecode.RVBool true);
  test "less than" "1 < 2" (Reg_bytecode.RVBool true);
  test "greater than" "2 > 1" (Reg_bytecode.RVBool true);
  test "if true" "if true then 1 else 2" (Reg_bytecode.RVInt 1);
  test "if false" "if false then 1 else 2" (Reg_bytecode.RVInt 2);
  test "let binding" "let x = 5 in x + 3" (Reg_bytecode.RVInt 8);
  test "string concat" "\"hello\" + \" world\"" (Reg_bytecode.RVString "hello world");
  test "recursion" "let rec fib = fun n -> if n <= 1 then n else fib (n - 1) + fib (n - 2) in fib 10" (Reg_bytecode.RVInt 55);
  test "while loop" "let i = ref 5 in let sum = ref 0 in while !i > 0 do sum := !sum + !i; i := !i - 1 done; !sum" (Reg_bytecode.RVInt 15);
  test "tail recursion" "let rec sum = fun n -> if n <= 0 then 0 else n + sum (n - 1) in sum 10000" (Reg_bytecode.RVInt 50005000);
  test "list literal" "[1, 2, 3]" (Reg_bytecode.RVList [Reg_bytecode.RVInt 1; Reg_bytecode.RVInt 2; Reg_bytecode.RVInt 3]);
  test "tuple literal" "(1, 2)" (Reg_bytecode.RVTuple [Reg_bytecode.RVInt 1; Reg_bytecode.RVInt 2]);
  Printf.printf "寄存器 VM 测试完成\n"
