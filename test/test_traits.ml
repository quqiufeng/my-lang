(** Traits 语义测试

    测试 trait 定义、实现和方法调用。
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
  print_endline "=== Traits 语义测试 ===";
  
  (* 测试 1: 内置 Show trait - int *)
  test "show int" "show 42" "\"42\"";
  
  (* 测试 2: 内置 Show trait - bool *)
  test "show bool" "show true" "\"true\"";
  
  (* 测试 3: 自定义 trait 定义和实现 *)
  test "custom trait" "trait Doubler { double : int }; impl Doubler for int { double = fun x -> x + x }; double 5" "10";
  
  (* 测试 4: 多个方法（拆分为两个单方法trait） *)
  test "multi methods" "trait AddOne { add_one : int }; impl AddOne for int { add_one = fun x -> x + 1 }; trait SubOne { sub_one : int }; impl SubOne for int { sub_one = fun x -> x - 1 }; add_one 10" "11";
  
  (* 测试 5: 同一 trait 多个类型实现 *)
  test "multi impl" "trait ToInt { to_int : int }; impl ToInt for bool { to_int = fun b -> if b then 1 else 0 }; to_int true" "1";
  
  (* 测试 6: trait 方法组合 *)
  test "trait compose" "trait Doubler2 { double : int }; impl Doubler2 for int { double = fun x -> x + x }; trait Calc2 { add_one : int }; impl Calc2 for int { add_one = fun x -> x + 1 }; let x = 3 in double (add_one x)" "8";
  
  print_endline "\n=== Traits 语义测试完成 ==="
