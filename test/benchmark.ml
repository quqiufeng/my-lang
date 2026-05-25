(** 性能基准测试

    比较不同执行模式的性能：
    1. 解释器 (eval)
    2. 栈式字节码 VM
    3. 寄存器 VM
*)

open Core
open My_lang

let benchmark name f n =
  (* 先验证正确性 *)
  let result = f () in
  Printf.printf "[%s result: %s] " name (My_lang.Ast.string_of_value result);
  let start = Time_float.now () in
  for _ = 1 to n do
    ignore (f ())
  done;
  let elapsed = Time_float.diff (Time_float.now ()) start in
  let ms = Time_float.Span.to_ms elapsed in
  Printf.printf "%s: %.2f ms (%.2f ms/op)\n" name ms (ms /. float_of_int n)

(** 斐波那契递归 - 计算 fib(20) *)
let fib_code = "
let rec fib = fun n ->
  if n <= 1 then n else fib (n - 1) + fib (n - 2)
in fib 20
"

(** 阶乘 - 计算 fact(100) *)
let fact_code = "
let rec fact = fun n ->
  if n <= 1 then 1 else n * fact (n - 1)
in fact 100
"

(** 列表求和 *)
let sum_code = "
let rec sum = fun xs ->
  match xs with
  | [] -> 0
  | h :: t -> h + sum t
in sum [1;2;3;4;5;6;7;8;9;10]
"

let run_eval code =
  let expr = My_lang.parse code in
  My_lang.eval expr

let run_bytecode code =
  let expr = My_lang.parse code in
  let bc = My_lang.compile expr in
  let result = My_lang.run_bytecode bc in
  (* 转换为 Ast.value 以便统一打印 *)
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

let () =
  Printf.printf "=== MyLang 性能基准测试 ===\n\n";
  
  (* 打印 fib 的寄存器字节码 *)
  let fib_expr = My_lang.parse fib_code in
  let fib_prog = Reg_compiler.compile_program [fib_expr] in
  Printf.printf "=== Fib 寄存器字节码 ===\n";
  print_endline (Reg_bytecode.disassemble fib_prog);
  Printf.printf "\n";
  
  let iterations = 100 in
  
  Printf.printf "-- 斐波那契(20) --\n";
  benchmark "解释器" (fun () -> run_eval fib_code) iterations;
  benchmark "栈式VM" (fun () -> run_bytecode fib_code) iterations;
  benchmark "寄存器VM" (fun () -> run_reg_vm fib_code) iterations;
  Printf.printf "\n";
  
  Printf.printf "-- 阶乘(100) --\n";
  benchmark "解释器" (fun () -> run_eval fact_code) iterations;
  benchmark "栈式VM" (fun () -> run_bytecode fact_code) iterations;
  benchmark "寄存器VM" (fun () -> run_reg_vm fact_code) iterations;
  Printf.printf "\n";
  
  Printf.printf "\n基准测试完成\n"
