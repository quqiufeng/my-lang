(** 测试 Scheme backend *)

open Ast
open Scheme_backend

(** 测试表达式 *)
let test_expr = EAdd (EInt 1, EInt 2)

(** 运行测试 *)
let () =
  Printf.printf "Testing Scheme backend...\n";
  Printf.printf "Expression: 1 + 2\n";
  Printf.printf "Scheme code: %s\n" (compile_expr test_expr);
  Printf.printf "Result: ";
  match compile_and_run test_expr with
  | Ok _ -> Printf.printf "Success!\n"
  | Error e -> Printf.printf "Error: %s\n" e
