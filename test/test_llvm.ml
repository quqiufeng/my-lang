(** LLVM IR 生成测试 *)

open Core
open My_lang

let test_llvm name code =
  let expr = My_lang.parse code in
  let prog = Reg_compiler.compile_program [expr] in
  let llvm_ir = Llvm_backend.generate_llvm_ir prog in
  
  Printf.printf "=== %s ===\n" name;
  Printf.printf "%s\n" llvm_ir;
  Printf.printf "\n"

let () =
  test_llvm "simple add" "1 + 2";
  test_llvm "let binding" "let x = 5 in x + 3";
  test_llvm "if expression" "if 1 < 2 then 100 else 200";
  test_llvm "function call" "let f = fun x -> x + 1 in f 5";
  
  Printf.printf "LLVM IR generation tests completed.\n"
