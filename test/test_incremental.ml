open Core
open My_lang

let () =
  Printf.printf "=== Incremental Compilation Tests ===\n";
  
  (* 测试1: 显示依赖图 *)
  Printf.printf "\n[Test 1] Show dependency graph\n";
  My_lang.Incremental_compile.show_dependency_graph "/tmp/test_inc_b.ml";
  
  (* 测试2: 首次增量编译 *)
  Printf.printf "\n[Test 2] First incremental compilation\n";
  Compilation_cache.clear_all_cache ();
  (match My_lang.Incremental_compile.compile_and_link ~cache:true "/tmp/test_inc_b.ml" with
  | Ok bytecode ->
      Printf.printf "Success! Bytecode length: %d\n" (Array.length bytecode)
  | Error msg ->
      Printf.printf "Failed: %s\n" msg);
  
  (* 测试3: 第二次编译（应该使用缓存） *)
  Printf.printf "\n[Test 3] Second compilation (should use cache)\n";
  (match My_lang.Incremental_compile.compile_and_link ~cache:true "/tmp/test_inc_b.ml" with
  | Ok bytecode ->
      Printf.printf "Success! Bytecode length: %d\n" (Array.length bytecode)
  | Error msg ->
      Printf.printf "Failed: %s\n" msg);
  
  (* 测试4: 修改依赖文件后编译 *)
  Printf.printf "\n[Test 4] Compile after modifying dependency\n";
  let oc = Stdlib.open_out "/tmp/test_inc_a.ml" in
  Stdlib.output_string oc "let add = fun x -> fun y -> x + y + 1 in\nlet mul = fun x -> fun y -> x * y in\nadd 1 2\n";
  Stdlib.close_out oc;
  
  (match My_lang.Incremental_compile.compile_and_link ~cache:true "/tmp/test_inc_b.ml" with
  | Ok bytecode ->
      Printf.printf "Success! Bytecode length: %d\n" (Array.length bytecode)
  | Error msg ->
      Printf.printf "Failed: %s\n" msg);
  
  (* 恢复文件 *)
  let oc = Stdlib.open_out "/tmp/test_inc_a.ml" in
  Stdlib.output_string oc "let add = fun x -> fun y -> x + y in\nlet mul = fun x -> fun y -> x * y in\nadd 1 2\n";
  Stdlib.close_out oc;
  
  Printf.printf "\n=== Incremental Compilation Tests Done ===\n"