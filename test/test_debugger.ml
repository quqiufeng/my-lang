(** 调试器测试 *)

open Core
open My_lang

let test_debugger name code =
  let expr = My_lang.parse code in
  let prog = Reg_compiler.compile_program [expr] in
  let state = Debugger.init_debug_state prog in
  
  Printf.printf "=== %s ===\n" name;
  Printf.printf "%s\n" (Debugger.disassemble_current state ~window:3);
  
  (* 运行到结束 *)
  let result = Debugger.run state in
  Printf.printf "Result: %s\n\n" (Reg_bytecode.string_of_reg_value result)

let test_breakpoint name code func_idx pc =
  let expr = My_lang.parse code in
  let prog = Reg_compiler.compile_program [expr] in
  let state = Debugger.init_debug_state prog in
  
  Debugger.set_breakpoint state func_idx pc;
  
  Printf.printf "=== %s (breakpoint at func=%d, pc=%d) ===\n" name func_idx pc;
  
  let result = Debugger.run state in
  if state.paused then (
    Printf.printf "Hit breakpoint at %s\n" (Debugger.string_of_location state);
    Printf.printf "%s\n" (Debugger.disassemble_current state ~window:2);
    
    (* 继续运行 *)
    let result2 = Debugger.continue state in
    Printf.printf "Final result: %s\n\n" (Reg_bytecode.string_of_reg_value result2)
  ) else
    Printf.printf "Result (no breakpoint hit): %s\n\n" (Reg_bytecode.string_of_reg_value result)

let test_step name code =
  let expr = My_lang.parse code in
  let prog = Reg_compiler.compile_program [expr] in
  let state = Debugger.init_debug_state prog in
  
  Printf.printf "=== %s (step) ===\n" name;
  
  (* 执行3步 *)
  for i = 1 to 3 do
    if Debugger.step_into state then (
      Printf.printf "Step %d: %s\n" i (Debugger.string_of_location state);
      Printf.printf "%s\n" (Debugger.disassemble_current state ~window:1);
    ) else
      Printf.printf "Step %d: finished\n" i
  done;
  Printf.printf "\n"

let () =
  test_debugger "simple add" "1 + 2";
  test_debugger "let binding" "let x = 5 in x + 3";
  
  (* 在 main 函数的第1条指令设置断点 *)
  test_breakpoint "breakpoint" "1 + 2" 0 1;
  
  test_step "step into" "let x = 5 in x + 3";
  
  Printf.printf "Debugger tests completed.\n"
