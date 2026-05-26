(** Actor 并发模型测试

    测试 spawn / send / receive。
*)

open Core
open My_lang

let test name code expected =
  try
    let result = My_lang.run code in
    let result_str = Ast.string_of_value result in
    if String.equal result_str expected then
      Printf.printf "[PASS] %s: %s\n" name result_str
    else
      Printf.printf "[FAIL] %s: got %s, expected %s\n" name result_str expected
  with exn ->
    Printf.printf "[FAIL] %s: exception %s\n" name (Exn.to_string exn)

let test_int name code =
  try
    let result = My_lang.run code in
    match result with
    | Ast.VInt n -> Printf.printf "[PASS] %s: %d\n" name n
    | _ -> Printf.printf "[FAIL] %s: got non-int\n" name
  with exn ->
    Printf.printf "[FAIL] %s: exception %s\n" name (Exn.to_string exn)

let () =
  print_endline "=== Actor 并发模型测试 ===";
  
  (* 测试 1: spawn 返回 pid (整数) *)
  test_int "spawn pid" "spawn (fun x -> x)";
  
  (* 测试 2: send + receive *)
  test "send receive" "let pid = spawn (fun x -> receive) in send pid 42; 42" "42";
  
  (* 测试 3: 多个 actor *)
  test_int "multi actor" "let p1 = spawn (fun x -> 1) in let p2 = spawn (fun x -> 2) in p1 + p2";
  
  (* 测试 4: actor 内部计算 *)
  test_int "actor compute" "spawn (fun x -> 10 + 20)";
  
  print_endline "\n=== Actor 并发模型测试完成 ==="
