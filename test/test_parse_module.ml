open Core
open My_lang

let () =
  print_endline "=== Module System Test ===";
  
  (* 测试 1: 模块定义和访问 *)
  let code1 = "module M = struct 42 end; M.__value" in
  (try
    let _expr1 = My_lang.parse code1 in
    print_endline ("Parsed: " ^ code1);
    let result1 = My_lang.run code1 in
    Printf.printf "Result: %s\n" (Ast.string_of_value result1)
  with
  | e -> Printf.printf "Error: %s\n" (Exn.to_string e));
  
  (* 测试 2: 模块中的 let *)
  let code2 = "module M = struct let x = 42 end; M.x" in
  (try
    let _expr2 = My_lang.parse code2 in
    print_endline ("Parsed: " ^ code2);
    let result2 = My_lang.run code2 in
    Printf.printf "Result: %s\n" (Ast.string_of_value result2)
  with
  | e -> Printf.printf "Error: %s\n" (Exn.to_string e));
  
  (* 测试 3: open 模块 *)
  let code3 = "module M = struct let y = 100 end; open M; y" in
  (try
    let _expr3 = My_lang.parse code3 in
    print_endline ("Parsed: " ^ code3);
    let result3 = My_lang.run code3 in
    Printf.printf "Result: %s\n" (Ast.string_of_value result3)
  with
  | e -> Printf.printf "Error: %s\n" (Exn.to_string e));
