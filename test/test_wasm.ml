open Core
open My_lang

let () =
  print_endline "=== Test 1: Basic Arithmetic ===";
  let code1 = "1 + 2 * 3" in
  let expr1 = My_lang.parse code1 in
  let wasm1 = My_lang.compile_to_wasm expr1 in
  print_endline wasm1;
  
  print_endline "\n=== Test 2: Variables and Conditionals ===";
  let code2 = "let x = 10 in if x > 5 then 100 else 0" in
  let expr2 = My_lang.parse code2 in
  let wasm2 = My_lang.compile_to_wasm expr2 in
  print_endline wasm2;
  
  print_endline "\n=== Test 3: Boolean Logic ===";
  let code3 = "true && false || true" in
  let expr3 = My_lang.parse code3 in
  let wasm3 = My_lang.compile_to_wasm expr3 in
  print_endline wasm3;
  
  print_endline "\n=== Test 4: Comparison ===";
  let code4 = "10 <= 20" in
  let expr4 = My_lang.parse code4 in
  let wasm4 = My_lang.compile_to_wasm expr4 in
  print_endline wasm4
