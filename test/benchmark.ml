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
let quicksort_code = "let rec append = fun xs -> fun ys -> match xs with | [] -> ys | x :: xs2 -> x :: append xs2 ys in let rec quicksort = fun xs -> match xs with | [] -> [] | p :: rest -> let rec partition = fun left -> fun right -> fun ys -> match ys with | [] -> (left, right) | y :: ys2 -> if y <= p then partition (y :: left) right ys2 else partition left (y :: right) ys2 in let parts = partition [] [] rest in match parts with | (left, right) -> append (quicksort left) (p :: quicksort right) in quicksort [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5]"
let tree_code = "type tree = | Leaf : int -> tree | Node : (tree * tree) -> tree; let rec tree_sum = fun t -> match t with | Leaf n -> n | Node (l, r) -> tree_sum l + tree_sum r in let rec build_tree = fun n -> if n = 0 then Leaf 1 else Node (build_tree (n - 1), build_tree (n - 1)) in tree_sum (build_tree 4)"
let gc_stress_code = "let rec make_list = fun n -> if n = 0 then [] else n :: make_list (n - 1) in let rec sum_list = fun xs -> match xs with | [] -> 0 | x :: xs2 -> x + sum_list xs2 in sum_list (make_list 1000)"

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

  Printf.printf "-- 快速排序(11个元素) --\n";
  benchmark "解释器" (fun () -> run_eval quicksort_code) iterations;
  benchmark "栈式VM" (fun () -> run_bytecode quicksort_code) iterations;
  benchmark "寄存器VM" (fun () -> run_reg_vm quicksort_code) iterations;
  Printf.printf "\n";

  Printf.printf "-- 二叉树求和(深度4) --\n";
  benchmark "解释器" (fun () -> run_eval tree_code) iterations;
  benchmark "栈式VM" (fun () -> run_bytecode tree_code) iterations;
  benchmark "寄存器VM" (fun () -> run_reg_vm tree_code) iterations;
  Printf.printf "\n";

  Printf.printf "-- GC压力测试(列表1000) --\n";
  benchmark "解释器" (fun () -> run_eval gc_stress_code) iterations;
  benchmark "栈式VM" (fun () -> run_bytecode gc_stress_code) iterations;
  benchmark "寄存器VM" (fun () -> run_reg_vm gc_stress_code) iterations;
  Printf.printf "\n";
  
  Printf.printf "基准测试完成\n"
