open Core
open My_lang

let () =
  print_endline "=== Test 1: Record update with existing field ===";
  let code1 = "let p = {name = \"Alice\"; age = 30} in {p with age = 31}" in
  let result1 = My_lang.run_exn code1 in
  (match result1 with
   | Ok v -> print_endline (Ast.string_of_value v)
   | Error msg -> print_endline ("Error: " ^ msg));
  
  print_endline "\n=== Test 2: Record update with new field ===";
  let code2 = "let p = {name = \"Bob\"} in {p with age = 25}" in
  let result2 = My_lang.run_exn code2 in
  (match result2 with
   | Ok v -> print_endline (Ast.string_of_value v)
   | Error msg -> print_endline ("Error: " ^ msg));
  
  print_endline "\n=== Test 3: Record update preserves original ===";
  let code3 = "let p = {name = \"Charlie\"; age = 20} in let p2 = {p with age = 21} in (p.name, p2.name, p.age, p2.age)" in
  let result3 = My_lang.run_exn code3 in
  (match result3 with
   | Ok v -> print_endline (Ast.string_of_value v)
   | Error msg -> print_endline ("Error: " ^ msg));
  
  print_endline "\n=== Test 4: Bytecode compile record update ===";
  let code4 = "let p = {name = \"Dave\"; age = 40} in {p with age = 41}" in
  let expr4 = My_lang.parse code4 in
  let bytecode = My_lang.compile expr4 in
  My_lang.Bytecode.print_code bytecode;
  
  print_endline "\n=== Test 5: Bytecode run record update ===";
  let vm_result = My_lang.run_bytecode bytecode in
  print_endline (My_lang.Vm.string_of_vm_value vm_result)
