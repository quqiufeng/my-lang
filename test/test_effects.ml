(** 效果系统测试

    测试 effect / perform / handle。
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

let () =
  print_endline "=== 效果系统测试 ===";
  
  (* 测试 1: 基本效果 State get *)
  test "state get" "effect State { get }; handle perform get 0 with { get x k -> k 42 }" "42";
  
  (* 测试 2: 效果 State set + get *)
  test "state set get" "effect State { get }; handle perform get 10 with { get x k -> k 20 }" "20";
  
  (* 测试 3: 效果操作组合 *)
  test "effect compose" "effect Add { add }; handle let a = perform add 3 in let b = perform add 4 in a + b with { add x k -> k (x + 1) }" "9";
  
  (* 测试 4: handler 直接返回值（不 resume） *)
  test "handler abort" "effect Abort { abort }; handle perform abort 0 with { abort x k -> 99 }" "99";
  
  print_endline "\n=== 效果系统测试完成 ==="
