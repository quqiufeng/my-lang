open Core
open My_lang

let () =
  print_endline "=== Test 1: Constructor ===";
  let code1 = "type 'a option = None | Some of 'a; Some 42" in
  let expr1 = My_lang.parse code1 in
  let wasm1 = My_lang.compile_to_wasm expr1 in
  print_endline wasm1;
  
  print_endline "\n=== Test 2: List ===";
  let code2 = "[1, 2, 3]" in
  let expr2 = My_lang.parse code2 in
  let wasm2 = My_lang.compile_to_wasm expr2 in
  print_endline wasm2;
  
  print_endline "\n=== Test 3: String ===";
  let code3 = "\"hello\"" in
  let expr3 = My_lang.parse code3 in
  let wasm3 = My_lang.compile_to_wasm expr3 in
  print_endline wasm3
