(** 综合性能基准测试

    覆盖更多场景的性能对比。
*)

open Core
open My_lang

let benchmark name f n =
  let result = f () in
  let start = Time_float.now () in
  for _ = 1 to n do
    ignore (f ())
  done;
  let elapsed = Time_float.diff (Time_float.now ()) start in
  let ms = Time_float.Span.to_ms elapsed in
  Printf.printf "[%s result: %s] %s: %.2f ms (%.2f ms/op)\n"
    name (My_lang.Ast.string_of_value result) name ms (ms /. float_of_int n)

let run_eval code =
  let expr = My_lang.parse code in
  My_lang.eval expr

let run_bytecode code =
  let expr = My_lang.parse code in
  let bc = My_lang.compile expr in
  let result = My_lang.run_bytecode bc in
  match result with
  | Vm.VInt n -> Ast.VInt n
  | Vm.VBool b -> Ast.VBool b
  | Vm.VString s -> Ast.VString s
  | _ -> Ast.VInt 0

let run_reg_vm code =
  let expr = My_lang.parse code in
  let prog = Reg_compiler.compile_program [expr] in
  let result = Reg_vm.execute prog in
  match result with
  | Reg_bytecode.RVInt n -> Ast.VInt n
  | Reg_bytecode.RVBool b -> Ast.VBool b
  | Reg_bytecode.RVString s -> Ast.VString s
  | _ -> Ast.VInt 0

(* 测试用例 *)
let fib_code = "let rec fib = fun n -> if n <= 1 then n else fib (n - 1) + fib (n - 2) in fib 20"
let fact_code = "let rec fact = fun n -> if n <= 1 then 1 else n * fact (n - 1) in fact 50"
let sum_code = "let rec sum = fun n -> if n <= 0 then 0 else n + sum (n - 1) in sum 1000"
let ack_code = "let rec ack = fun m -> fun n -> if m = 0 then n + 1 else if n = 0 then ack (m - 1) 1 else ack (m - 1) (ack m (n - 1)) in ack 3 6"
let while_code = "let i = ref 100000 in let sum = ref 0 in while !i > 0 do sum := !sum + 1; i := !i - 1 done; !sum"
let nested_if_code = "let rec f = fun n -> if n <= 0 then 0 else if n = 1 then 1 else if n = 2 then 2 else f (n - 1) + f (n - 2) in f 25"

let () =
  Printf.printf "=== MyLang 综合性能基准测试 ===\n\n";
  let iterations = 50 in
  
  Printf.printf "-- 斐波那契(20) --\n";
  benchmark "解释器" (fun () -> run_eval fib_code) iterations;
  benchmark "栈式VM" (fun () -> run_bytecode fib_code) iterations;
  benchmark "寄存器VM" (fun () -> run_reg_vm fib_code) iterations;
  Printf.printf "\n";
  
  Printf.printf "-- 阶乘(50) --\n";
  benchmark "解释器" (fun () -> run_eval fact_code) iterations;
  benchmark "栈式VM" (fun () -> run_bytecode fact_code) iterations;
  benchmark "寄存器VM" (fun () -> run_reg_vm fact_code) iterations;
  Printf.printf "\n";
  
  Printf.printf "-- 求和(1000) --\n";
  benchmark "解释器" (fun () -> run_eval sum_code) iterations;
  benchmark "栈式VM" (fun () -> run_bytecode sum_code) iterations;
  benchmark "寄存器VM" (fun () -> run_reg_vm sum_code) iterations;
  Printf.printf "\n";
  
  Printf.printf "-- Ackermann(3,6) --\n";
  benchmark "解释器" (fun () -> run_eval ack_code) iterations;
  benchmark "栈式VM" (fun () -> run_bytecode ack_code) iterations;
  benchmark "寄存器VM" (fun () -> run_reg_vm ack_code) iterations;
  Printf.printf "\n";
  
  Printf.printf "-- While循环(10万次) --\n";
  benchmark "解释器" (fun () -> run_eval while_code) 10;
  benchmark "栈式VM" (fun () -> run_bytecode while_code) 10;
  benchmark "寄存器VM" (fun () -> run_reg_vm while_code) 10;
  Printf.printf "\n";
  
  Printf.printf "-- 嵌套If(25) --\n";
  benchmark "解释器" (fun () -> run_eval nested_if_code) iterations;
  benchmark "栈式VM" (fun () -> run_bytecode nested_if_code) iterations;
  benchmark "寄存器VM" (fun () -> run_reg_vm nested_if_code) iterations;
  Printf.printf "\n";
  
  Printf.printf "基准测试完成\n"
