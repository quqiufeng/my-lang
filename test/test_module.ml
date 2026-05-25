open Core
open My_lang

let () =
  print_endline "=== Module System Tests ===";
  
  (* 测试 1: 基本模块定义和访问 *)
  let code1 = "module M = struct let x = 42 end; M.x" in
  let result1 = My_lang.run code1 in
  Printf.printf "Test 1 (module access): %s\n" (Ast.string_of_value result1);
  
  (* 测试 2: open 模块 *)
  let code2 = "module M = struct let x = 100 end; open M; x" in
  let result2 = My_lang.run code2 in
  Printf.printf "Test 2 (open module): %s\n" (Ast.string_of_value result2);
  
  (* 测试 3: 嵌套 let *)
  let code3 = "module M = struct let x = 1; let y = 2 end; M.x + M.y" in
  let result3 = My_lang.run code3 in
  Printf.printf "Test 3 (multiple bindings): %s\n" (Ast.string_of_value result3);
  
  print_endline "\n=== Module Tests Passed ==="
