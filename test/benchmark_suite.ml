open Core
open My_lang

(** 性能基准测试框架 *)

let iterations = 100

(** 测量执行时间 *)
let measure_time name f =
  let start = Core.Time_float.now () in
  let result = f () in
  let elapsed = Core.Time_float.diff (Core.Time_float.now ()) start in
  let ms = Core.Time_float.Span.to_ms elapsed in
  Printf.printf "  %-30s %8.2f ms (%8.4f ms/op)\n" name ms (ms /. float_of_int iterations);
  result

(** 运行基准测试 *)
let run_benchmark name code =
  try
    let expr = My_lang.parse code in
    let _ = measure_time name (fun () ->
      for _ = 1 to iterations do
        ignore (My_lang.eval expr)
      done
    ) in
    ()
  with exn ->
    Printf.printf "  %-30s ERROR: %s\n" name (Exn.to_string exn)

(** 运行解释器基准测试 *)
let run_interpreter_benchmark name code =
  try
    let _ = measure_time name (fun () ->
      for _ = 1 to iterations do
        ignore (My_lang.run code)
      done
    ) in
    ()
  with exn ->
    Printf.printf "  %-30s ERROR: %s\n" name (Exn.to_string exn)

let () =
  Printf.printf "=== MyLang Performance Benchmark ===\n\n";
  Printf.printf "Iterations: %d\n\n" iterations;
  
  (* ===== 算术运算 ===== *)
  Printf.printf "-- 算术运算 --\n";
  run_interpreter_benchmark "简单加法" "1 + 2";
  run_interpreter_benchmark "复杂表达式" "(1 + 2) * (3 + 4) - (5 + 6) / (7 + 8)";
  run_interpreter_benchmark "嵌套括号" "((((1 + 2) + 3) + 4) + 5)";
  
  (* ===== 变量绑定 ===== *)
  Printf.printf "\n-- 变量绑定 --\n";
  run_interpreter_benchmark "简单 let" "let x = 42 in x";
  run_interpreter_benchmark "嵌套 let" "let x = 1 in let y = 2 in let z = 3 in x + y + z";
  run_interpreter_benchmark "多绑定" "let a = 1 in let b = 2 in let c = 3 in let d = 4 in a + b + c + d";
  
  (* ===== 函数调用 ===== *)
  Printf.printf "\n-- 函数调用 --\n";
  run_interpreter_benchmark "简单函数" "let f = fun x -> x + 1 in f 42";
  run_interpreter_benchmark "高阶函数" "let f = fun x -> fun y -> x + y in f 1 2";
  run_interpreter_benchmark "递归函数" "let rec fib = fun n -> if n <= 1 then n else fib (n - 1) + fib (n - 2) in fib 10";
  
  (* ===== 列表操作 ===== *)
  Printf.printf "\n-- 列表操作 --\n";
  run_interpreter_benchmark "列表创建" "[1, 2, 3, 4, 5]";
  run_interpreter_benchmark "列表连接" "append [1, 2] [3, 4, 5]";
  run_interpreter_benchmark "列表反转" "reverse [1, 2, 3, 4, 5]";
  run_interpreter_benchmark "列表排序" "sort [5, 3, 1, 4, 2]";
  run_interpreter_benchmark "列表映射" "map (fun x -> x + 1) [1, 2, 3, 4, 5]";
  run_interpreter_benchmark "列表过滤" "filter (fun x -> x > 2) [1, 2, 3, 4, 5]";
  run_interpreter_benchmark "列表折叠" "fold (fun acc -> fun x -> acc + x) 0 [1, 2, 3, 4, 5]";
  
  (* ===== 字符串操作 ===== *)
  Printf.printf "\n-- 字符串操作 --\n";
  run_interpreter_benchmark "字符串连接" "\"hello\" ^ \" \" ^ \"world\"";
  run_interpreter_benchmark "字符串长度" "string_length \"hello world\"";
  run_interpreter_benchmark "字符串分割" "string_split (\",\", \"a,b,c,d,e\")";
  run_interpreter_benchmark "字符串替换" "string_replace (\"l\", \"r\", \"hello world\")";
  
  (* ===== HashMap 操作 ===== *)
  Printf.printf "\n-- HashMap 操作 --\n";
  run_interpreter_benchmark "HashMap 创建" "hashmap_create ()";
  run_interpreter_benchmark "HashMap 设置" "hashmap_set (hashmap_create (), \"x\", 42)";
  run_interpreter_benchmark "HashMap 获取" "hashmap_get (hashmap_set (hashmap_create (), \"x\", 42), \"x\")";
  
  (* ===== JSON 操作 ===== *)
  Printf.printf "\n-- JSON 操作 --\n";
  run_interpreter_benchmark "JSON 解析" "json_parse \"{\\\"x\\\": 1, \\\"y\\\": 2}\"";
  run_interpreter_benchmark "JSON 序列化" "json_stringify [1, 2, 3]";
  
  (* ===== 日期时间 ===== *)
  Printf.printf "\n-- 日期时间 --\n";
  run_interpreter_benchmark "获取时间" "time_now ()";
  run_interpreter_benchmark "格式化时间" "time_format (1704067200, \"%Y-%m-%d %H:%M:%S\")";
  
  (* ===== 控制流 ===== *)
  Printf.printf "\n-- 控制流 --\n";
  run_interpreter_benchmark "条件表达式" "if true then 1 else 2";
  run_interpreter_benchmark "模式匹配" "match [1, 2, 3] with | [] -> 0 | x :: rest -> x";
  run_interpreter_benchmark "While 循环" "let i = ref 0 in while !i < 100 do i := !i + 1 done; !i";
  
  (* ===== 递归 ===== *)
  Printf.printf "\n-- 递归 --\n";
  run_interpreter_benchmark "阶乘" "let rec fact = fun n -> if n <= 1 then 1 else n * fact (n - 1) in fact 20";
  run_interpreter_benchmark "斐波那契" "let rec fib = fun n -> if n <= 1 then n else fib (n - 1) + fib (n - 2) in fib 20";
  run_interpreter_benchmark "快速排序" "let rec quicksort = fun xs -> match xs with | [] -> [] | p :: rest -> let left = filter (fun x -> x < p) rest in let right = filter (fun x -> x >= p) rest in append (quicksort left) (p :: quicksort right) in quicksort [5, 3, 1, 4, 2]";
  
  Printf.printf "\n=== 基准测试完成 ===\n"
