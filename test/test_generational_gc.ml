(** 分代 GC 集成测试

    测试 GC 与 eval/VM 的集成。
*)

open Core
open My_lang

let () =
  print_endline "=== 分代 GC 集成测试 ===";
  
  (* 重置 GC 状态 *)
  Gc_bridge.reset ();
  
  (* 测试 1: 解释器带 GC *)
  let result1, stats1 = My_lang.run_with_gc "[1, 2, 3]" in
  Printf.printf "解释器 [1,2,3]: %s | %s\n" (Ast.string_of_value result1) stats1;
  
  (* 测试 2: 栈 VM 带 GC *)
  let expr = My_lang.parse "[10, 20, 30]" in
  let bc = My_lang.compile expr in
  let result2, stats2 = My_lang.run_bytecode_with_gc bc in
  Printf.printf "栈VM [10,20,30]: %s | %s\n" (Vm.string_of_vm_value result2) stats2;
  
  (* 测试 3: 寄存器 VM 带 GC *)
  let expr = My_lang.parse "[100, 200, 300]" in
  let prog = Reg_compiler.compile_program [expr] in
  let result3, stats3 = My_lang.run_reg_vm_with_gc prog in
  Printf.printf "寄存器VM [100,200,300]: %s | %s\n" (Reg_bytecode.string_of_reg_value result3) stats3;
  
  (* 测试 4: 元组 *)
  let result4, stats4 = My_lang.run_with_gc "(1, 2, 3)" in
  Printf.printf "解释器 (1,2,3): %s | %s\n" (Ast.string_of_value result4) stats4;
  
  (* 测试 5: 引用 *)
  let result5, stats5 = My_lang.run_with_gc "ref 42" in
  Printf.printf "解释器 ref 42: %s | %s\n" (Ast.string_of_value result5) stats5;
  
  (* 测试 6: 大量对象触发 GC *)
  Gc_bridge.reset ();
  let code = "let rec build = fun n -> if n <= 0 then [] else n :: build (n - 1) in build 50" in
  let result6, stats6 = My_lang.run_with_gc code in
  Printf.printf "大量对象 build 50: %s | %s\n" (Ast.string_of_value result6) stats6;
  
  print_endline "\n=== 分代 GC 集成测试完成 ==="
