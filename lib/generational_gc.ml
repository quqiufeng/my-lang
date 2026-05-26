(** 分代垃圾回收器

    实现分代 GC，替代原有的标记-清除：
    - 年轻代（Young Generation）：新对象，使用复制算法
    - 老年代（Old Generation）：存活对象，使用标记-清除-压缩
    
    对象晋升：经历 N 次 GC 后存活的对象晋升到老年代。
*)

open Core

(** 堆对象 *)
type heap_obj = {
  mutable marked : bool;
  mutable generation : int;  (* 0=年轻代, 1=老年代 *)
  mutable age : int;         (* GC 存活次数 *)
  mutable data : obj_data;
}

and obj_data =
  | OInt of int
  | OBool of bool
  | OString of string
  | OList of heap_obj list
  | OTuple of heap_obj array
  | OClosure of (heap_obj list -> heap_obj)
  | ORecord of (string * heap_obj) list
  | ORef of heap_obj ref
  | OArray of heap_obj array

(** 分代堆 *)
type generational_heap = {
  mutable young_size : int;
  mutable young_capacity : int;
  mutable young : heap_obj list;
  mutable old : heap_obj list;
  mutable gc_count : int;
  mutable promotions : int;
  mutable collections : int;
}

let create_heap ~young_capacity () = {
  young_size = 0;
  young_capacity;
  young = [];
  old = [];
  gc_count = 0;
  promotions = 0;
  collections = 0;
}

(** 分配对象到年轻代 *)
let allocate heap data =
  let obj = { marked = false; generation = 0; age = 0; data } in
  heap.young <- obj :: heap.young;
  heap.young_size <- heap.young_size + 1;
  obj

(** 年轻代 GC：复制算法 *)
let minor_gc heap roots =
  let survivors = ref [] in
  
  (* 标记存活对象 *)
  let rec mark obj =
    if not obj.marked then (
      obj.marked <- true;
      match obj.data with
      | OList objs ->
          List.iter objs ~f:mark
      | OTuple objs ->
          Array.iter objs ~f:mark
      | ORecord fields ->
          List.iter fields ~f:(fun (_, v) -> mark v)
      | _ -> ())
  in
  
  (* 从根开始标记 *)
  List.iter roots ~f:mark;
  
  (* 复制存活对象 *)
  List.iter heap.young ~f:(fun obj ->
    if obj.marked then (
      obj.age <- obj.age + 1;
      if obj.age >= 3 then (
        (* 晋升到老年代 *)
        obj.generation <- 1;
        heap.old <- obj :: heap.old;
        heap.promotions <- heap.promotions + 1;
      ) else
        survivors := obj :: !survivors;
      obj.marked <- false;
    ));
  
  heap.young <- !survivors;
  heap.young_size <- List.length !survivors;
  heap.gc_count <- heap.gc_count + 1

(** 老年代 GC：标记-清除 *)
let major_gc heap roots =
  (* 标记 *)
  let rec mark obj =
    if not obj.marked then (
      obj.marked <- true;
      match obj.data with
      | OList objs -> List.iter objs ~f:mark
      | OTuple objs -> Array.iter objs ~f:mark
      | ORecord fields ->
          List.iter fields ~f:(fun (_, v) -> mark v)
      | _ -> ())
  in
  
  List.iter roots ~f:mark;
  
  (* 清除 *)
  heap.old <- List.filter heap.old ~f:(fun obj ->
    let alive = obj.marked in
    obj.marked <- false;
    alive);
  
  heap.collections <- heap.collections + 1

(** 触发 GC *)
let gc heap roots =
  if heap.young_size >= heap.young_capacity then (
    minor_gc heap roots;
    if heap.gc_count mod 10 = 0 then
      major_gc heap roots;
  )

(** 堆统计 *)
let heap_stats heap =
  Printf.sprintf "GC统计: 年轻代=%d, 老年代=%d, GC次数=%d, 晋升=%d, 完全回收=%d"
    heap.young_size
    (List.length heap.old)
    heap.gc_count
    heap.promotions
    heap.collections

(** 创建整数字 *)
let make_int heap n = allocate heap (OInt n)

(** 创建布尔对象 *)
let make_bool heap b = allocate heap (OBool b)

(** 创建字符串对象 *)
let make_string heap s = allocate heap (OString s)

(** 创建列表对象 *)
let make_list heap objs = allocate heap (OList objs)

(** 创建元组对象 *)
let make_tuple heap objs = allocate heap (OTuple (Array.of_list objs))

(** 创建引用对象 *)
let make_ref heap obj = allocate heap (ORef (ref obj))

(** 创建数组对象 *)
let make_array heap objs = allocate heap (OArray (Array.of_list objs))

(** 强制触发 GC（忽略阈值） *)
let force_gc heap roots =
  minor_gc heap roots;
  if heap.gc_count mod 10 = 0 then
    major_gc heap roots

(** 获取存活对象总数 *)
let alive_count heap =
  heap.young_size + List.length heap.old

(** 重置堆 *)
let reset_heap heap =
  heap.young <- [];
  heap.young_size <- 0;
  heap.old <- [];
  heap.gc_count <- 0;
  heap.promotions <- 0;
  heap.collections <- 0
