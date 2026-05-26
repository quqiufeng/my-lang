(** GC 桥接层

    提供全局 GC 堆实例和统计接口。
    VM 层直接使用 Generational_gc 模块分配对象。
*)

open Core

(** 全局 GC 堆 *)
let global_heap = lazy (Generational_gc.create_heap ~young_capacity:100 ())

let get_heap () = Lazy.force global_heap

(** 分配计数，用于触发 GC *)
let alloc_counter = ref 0
let alloc_threshold = ref 50

(** 跟踪对象分配，自动触发 GC *)
let track_alloc obj =
  incr alloc_counter;
  if !alloc_counter >= !alloc_threshold then (
    alloc_counter := 0;
    (* GC 由调用者显式触发，这里只重置计数器 *)
  )

(** 获取 GC 统计 *)
let stats () = Generational_gc.heap_stats (get_heap ())

(** 重置 GC 状态 *)
let reset () =
  alloc_counter := 0;
  Generational_gc.reset_heap (get_heap ())

(** 强制触发 GC *)
let force_gc roots =
  Generational_gc.force_gc (get_heap ()) roots
