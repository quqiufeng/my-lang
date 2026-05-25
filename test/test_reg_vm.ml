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
  Printf.printf "寄存器 VM 测试完成\n"
