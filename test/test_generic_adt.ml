open Core
open My_lang

let () =
  print_endline "=== Test 1: Generic option with int ===";
  let code1 = "type 'a option = None | Some of 'a; let x = Some 42 in match x with | Some n -> n | None -> 0" in
  (match My_lang.run_exn code1 with
   | Ok v -> print_endline (Ast.string_of_value v)
   | Error msg -> print_endline ("Error: " ^ msg));
  
  print_endline "\n=== Test 2: Generic option simple ===";
  let code2 = "type 'a option = None | Some of 'a; Some 42" in
  (match My_lang.run_exn code2 with
   | Ok v -> print_endline (Ast.string_of_value v)
   | Error msg -> print_endline ("Error: " ^ msg));
  
  print_endline "\n=== Test 3: Generic result ===";
  let code3 = "type ('a, 'b) result = Ok of 'a | Error of 'b; Ok 42" in
  (match My_lang.run_exn code3 with
   | Ok v -> print_endline (Ast.string_of_value v)
   | Error msg -> print_endline ("Error: " ^ msg));
  
  print_endline "\n=== Test 4: Bytecode compile ===";
  let code4 = "type 'a option = None | Some of 'a; Some 42" in
  let expr4 = My_lang.parse code4 in
  let bytecode = My_lang.compile expr4 in
  My_lang.Bytecode.print_code bytecode;
  
  print_endline "\n=== Test 5: Bytecode run ===";
  let vm_result = My_lang.run_bytecode bytecode in
  print_endline (My_lang.Vm.string_of_vm_value vm_result)
