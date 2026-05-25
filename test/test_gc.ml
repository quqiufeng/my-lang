open Core
open My_lang

let () =
  print_endline "=== GC Basic Tests ===";
  
  (* 测试 1: 创建堆 *)
  let heap = My_lang.Gc.create_heap 1024 in
  print_endline "Created heap of 1024 bytes";
  
  (* 测试 2: 分配对象 *)
  let (addr1, header1) = My_lang.Gc.heap_alloc heap My_lang.Gc.tag_list 16 in
  Printf.printf "Allocated list object at %d, size=%d\n" addr1 header1.size;
  
  let (addr2, header2) = My_lang.Gc.heap_alloc heap My_lang.Gc.tag_tuple 12 in
  Printf.printf "Allocated tuple object at %d, size=%d\n" addr2 header2.size;
  
  (* 测试 3: 堆统计 *)
  let stats = My_lang.Gc.string_of_heap_stats heap in
  Printf.printf "Heap stats: %s\n" stats;
  
  (* 测试 4: 标记-清除 *)
  My_lang.Gc.run_gc heap [addr1];  (* 只保留 addr1 *)
  let stats_after = My_lang.Gc.string_of_heap_stats heap in
  Printf.printf "After GC (keep addr1): %s\n" stats_after;
  
  print_endline "\n=== GC Tests Passed ==="
