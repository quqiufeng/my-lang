open Core
open My_lang

let timeit f =
  let start = Core.Time_ns.now () in
  let result = f () in
  let elapsed = Core.Time_ns.diff (Core.Time_ns.now ()) start |> Core.Time_ns.Span.to_ms in
  (result, elapsed)

let () =
  Printf.printf "=== AST Cache Performance Test ===\n\n";

  (* 准备较大的测试文件 *)
  let big_module_a = "/tmp/test_big_a.ml" in
  let big_module_b = "/tmp/test_big_b.ml" in
  
  let oc = Stdlib.open_out big_module_a in
  for i = 1 to 100 do
    Stdlib.output_string oc (Printf.sprintf "let f%d = fun x -> x + %d in\n" i i)
  done;
  Stdlib.output_string oc "let add = fun x -> fun y -> x + y in\nadd 1 2\n";
  Stdlib.close_out oc;
  
  let oc = Stdlib.open_out big_module_b in
  Stdlib.output_string oc (Printf.sprintf "import \"%s\";\n" big_module_a);
  Stdlib.output_string oc "let result = add 100 200 in\nresult\n";
  Stdlib.close_out oc;

  (* 清除缓存 *)
  Compilation_cache.clear_all_cache ();
  
  (* 测试1: 无 AST 缓存的首次编译 *)
  Printf.printf "[Cold] First compilation (no AST cache)...\n";
  let (_, t1) = timeit (fun () ->
    match Incremental_compile.compile_and_link ~cache:true big_module_b with
    | Ok _ -> ()
    | Error msg -> Printf.printf "Error: %s\n" msg
  ) in
  Printf.printf "  Time: %.3f ms\n\n" t1;
  
  (* 测试2: 有 AST 缓存的第二次编译 *)
  Printf.printf "[Warm] Second compilation (with AST cache)...\n";
  let (_, t2) = timeit (fun () ->
    match Incremental_compile.compile_and_link ~cache:true big_module_b with
    | Ok _ -> ()
    | Error msg -> Printf.printf "Error: %s\n" msg
  ) in
  Printf.printf "  Time: %.3f ms\n" t2;
  Printf.printf "  Speedup: %.1fx\n\n" (t1 /. t2);
  
  (* 测试3: 修改依赖后（A 需要重新 parse，B 用 AST 缓存） *)
  Printf.printf "[Modified] Compile after modifying dependency A...\n";
  let oc = Stdlib.open_out big_module_a in
  for i = 1 to 100 do
    Stdlib.output_string oc (Printf.sprintf "let f%d = fun x -> x + %d in\n" i (i+1))
  done;
  Stdlib.output_string oc "let add = fun x -> fun y -> x + y in\nadd 1 2\n";
  Stdlib.close_out oc;
  
  let (_, t3) = timeit (fun () ->
    match Incremental_compile.compile_and_link ~cache:true big_module_b with
    | Ok _ -> ()
    | Error msg -> Printf.printf "Error: %s\n" msg
  ) in
  Printf.printf "  Time: %.3f ms\n\n" t3;
  
  (* 测试4: 清除 AST 缓存后的编译 *)
  Printf.printf "[No-Cache] Compilation after clearing AST cache...\n";
  Compilation_cache.clear_all_cache ();
  let (_, t4) = timeit (fun () ->
    match Incremental_compile.compile_and_link ~cache:true big_module_b with
    | Ok _ -> ()
    | Error msg -> Printf.printf "Error: %s\n" msg
  ) in
  Printf.printf "  Time: %.3f ms\n" t4;
  Printf.printf "  Overhead vs AST cache: %.1fx\n\n" (t4 /. t2);
  
  Printf.printf "=== AST Cache Performance Test Done ===\n"