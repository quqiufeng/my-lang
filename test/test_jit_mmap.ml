(** JIT mmap 测试

    测试真实 RWX 内存分配和机器码执行。
*)

open Core
open My_lang

let () =
  print_endline "=== JIT mmap 测试 ===";
  
  (* 测试 1: 简单常量 *)
  let expr1 = My_lang.parse "42" in
  let prog1 = Reg_compiler.compile_program [expr1] in
  let result1 = Jit.execute_jit prog1 in
  Printf.printf "JIT 42: %s\n" (Reg_bytecode.string_of_reg_value result1);
  
  (* 测试 2: 加法 *)
  let expr2 = My_lang.parse "1 + 2" in
  let prog2 = Reg_compiler.compile_program [expr2] in
  let result2 = Jit.execute_jit prog2 in
  Printf.printf "JIT 1+2: %s\n" (Reg_bytecode.string_of_reg_value result2);
  
  (* 测试 3: 减法 *)
  let expr3 = My_lang.parse "5 - 3" in
  let prog3 = Reg_compiler.compile_program [expr3] in
  let result3 = Jit.execute_jit prog3 in
  Printf.printf "JIT 5-3: %s\n" (Reg_bytecode.string_of_reg_value result3);
  
  (* 测试 4: 乘法 *)
  let expr4 = My_lang.parse "3 * 4" in
  let prog4 = Reg_compiler.compile_program [expr4] in
  let result4 = Jit.execute_jit prog4 in
  Printf.printf "JIT 3*4: %s\n" (Reg_bytecode.string_of_reg_value result4);
  
  (* 测试 5: 除法 *)
  let expr5 = My_lang.parse "8 / 2" in
  let prog5 = Reg_compiler.compile_program [expr5] in
  let result5 = Jit.execute_jit prog5 in
  Printf.printf "JIT 8/2: %s\n" (Reg_bytecode.string_of_reg_value result5);
  
  (* 测试 6: 复杂表达式 *)
  let expr6 = My_lang.parse "(1 + 2) * (3 + 4)" in
  let prog6 = Reg_compiler.compile_program [expr6] in
  let result6 = Jit.execute_jit prog6 in
  Printf.printf "JIT (1+2)*(3+4): %s\n" (Reg_bytecode.string_of_reg_value result6);
  
  print_endline "\n=== JIT mmap 测试完成 ==="
