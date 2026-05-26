(** 并行编译测试 *)

open Core
open My_lang

let () =
  Printf.printf "=== Parallel Compilation Tests ===\n";

  (* 创建测试文件：独立的模块，没有跨模块变量引用 *)
  let module_a = "/tmp/test_par_a.ml" in
  let module_b = "/tmp/test_par_b.ml" in
  let module_c = "/tmp/test_par_c.ml" in
  let entry = "/tmp/test_par_entry.ml" in

  let oc_a = Stdlib.open_out module_a in
  Stdlib.output_string oc_a "let x = 42 in\nx + 1\n";
  Stdlib.close_out oc_a;

  let oc_b = Stdlib.open_out module_b in
  Stdlib.output_string oc_b "let y = 100 in\ny + 2\n";
  Stdlib.close_out oc_b;

  let oc_c = Stdlib.open_out module_c in
  Stdlib.output_string oc_c "let z = 200 in\nz + 3\n";
  Stdlib.close_out oc_c;

  let oc_entry = Stdlib.open_out entry in
  Stdlib.output_string oc_entry (Printf.sprintf "import \"%s\";\n" module_a);
  Stdlib.output_string oc_entry (Printf.sprintf "import \"%s\";\n" module_b);
  Stdlib.output_string oc_entry (Printf.sprintf "import \"%s\";\n" module_c);
  Stdlib.output_string oc_entry "42\n";
  Stdlib.close_out oc_entry;

  (* 测试串行编译 *)
  Printf.printf "\n[Test 1] Serial compilation\n";
  Compilation_cache.clear_all_cache ();
  let t1 = Core.Time_ns.now () in
  (match Incremental_compile.compile_and_link ~cache:true entry with
   | Ok bytecode ->
       let elapsed = Core.Time_ns.Span.to_ms (Core.Time_ns.diff (Core.Time_ns.now ()) t1) in
       Printf.printf "Serial: Success! %d instructions (%.2f ms)\n" (Array.length bytecode) elapsed
   | Error msg ->
       Printf.printf "Serial: Failed: %s\n" msg);

  (* 测试并行编译 *)
  Printf.printf "\n[Test 2] Parallel compilation\n";
  Compilation_cache.clear_all_cache ();
  let t2 = Core.Time_ns.now () in
  (match Incremental_compile.compile_and_link_parallel ~cache:true entry with
   | Ok bytecode ->
       let elapsed = Core.Time_ns.Span.to_ms (Core.Time_ns.diff (Core.Time_ns.now ()) t2) in
       Printf.printf "Parallel: Success! %d instructions (%.2f ms)\n" (Array.length bytecode) elapsed
   | Error msg ->
       Printf.printf "Parallel: Failed: %s\n" msg);

  (* 测试缓存复用（并行） *)
  Printf.printf "\n[Test 3] Parallel compilation with cache\n";
  let t3 = Core.Time_ns.now () in
  (match Incremental_compile.compile_and_link_parallel ~cache:true entry with
   | Ok bytecode ->
       let elapsed = Core.Time_ns.Span.to_ms (Core.Time_ns.diff (Core.Time_ns.now ()) t3) in
       Printf.printf "Parallel (cached): Success! %d instructions (%.2f ms)\n" (Array.length bytecode) elapsed
   | Error msg ->
       Printf.printf "Parallel (cached): Failed: %s\n" msg);

  (* 测试依赖图分层 *)
  Printf.printf "\n[Test 4] Topological levels\n";
  let graph = Module_dependency.create_graph () in
  let visited = Hashtbl.create (module String) in
  Module_dependency.build_graph graph entry visited;
  let levels = Module_dependency.topological_levels graph in
  Printf.printf "Levels (%d):\n" (List.length levels);
  List.iteri levels ~f:(fun i level ->
    Printf.printf "  Level %d: %s\n" i (String.concat ~sep:", " level));

  (* 验证层级顺序：同一层级内的模块没有互相依赖 *)
  let all_ok = ref true in
  List.iter levels ~f:(fun level ->
    List.iter level ~f:(fun name ->
      let deps = match Hashtbl.find graph.adjacency name with
        | Some d -> d
        | None -> [] in
      List.iter deps ~f:(fun dep ->
        if List.mem level dep ~equal:String.equal then (
          Printf.eprintf "ERROR: %s depends on %s but they are in the same level\n" name dep;
          all_ok := false
        )
      )
    )
  );
  if !all_ok then Printf.printf "Level consistency: OK\n";

  Printf.printf "\n=== Parallel Compilation Tests Done ===\n"
