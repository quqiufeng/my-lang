(** 垃圾回收器 - Mark-Sweep 算法

    简单的标记-清除垃圾回收器，用于管理 VM 的堆内存。
    
    设计：
    - 使用引用计数 + 标记清除的混合策略
    - 堆内存分为对象头 + 数据
    - GC 在内存分配失败或达到阈值时触发
*)

open Ast

(** 堆对象头 *)
type obj_header = {
  mutable marked : bool;        (* GC 标记位 *)
  mutable size : int;           (* 对象大小（字节） *)
  mutable tag : int;            (* 对象类型标签 *)
}

(** 堆对象标签 *)
let tag_list = 1
let tag_tuple = 2
let tag_closure = 3
let tag_ctor = 4
let tag_array = 5
let tag_record = 6
let tag_string = 7
let tag_ref = 8

(** 堆管理器 *)
type heap = {
  mutable memory : bytes;       (* 原始内存 *)
  mutable heap_size : int;      (* 堆大小 *)
  mutable free_ptr : int;       (* 下一个空闲位置 *)
  mutable allocated : int;      (* 已分配字节数 *)
  mutable threshold : int;      (* GC 阈值 *)
  mutable objects : (int * obj_header) list;  (* 已分配对象列表 (地址, 头) *)
}

(** 创建堆 *)
let create_heap size = {
  memory = Bytes.create size;
  heap_size = size;
  free_ptr = 0;
  allocated = 0;
  threshold = size / 2;  (* 默认阈值：50% *)
  objects = [];
}

(** 对齐到 4 字节边界 *)
let align4 n = (n + 3) land (lnot 3)

(** 在堆上分配内存 *)
let heap_alloc heap tag size =
  let total_size = align4 (8 + size) in  (* 8 字节头部 + 数据 *)
  
  (* 检查是否需要 GC *)
  if heap.free_ptr + total_size > heap.heap_size || heap.allocated > heap.threshold then
    failwith "GC needed";  (* 触发 GC 的信号 *)
  
  let addr = heap.free_ptr in
  let header = { marked = false; size = total_size; tag = tag } in
  
  (* 写入头部 *)
  Bytes.set_int32_le heap.memory addr 0l;  (* marked = false *)
  Bytes.set_int32_le heap.memory (addr + 4) (Int32.of_int total_size);
  
  heap.free_ptr <- heap.free_ptr + total_size;
  heap.allocated <- heap.allocated + total_size;
  heap.objects <- (addr, header) :: heap.objects;
  
  (addr, header)

(** 释放对象 *)
let heap_free heap addr =
  heap.objects <- List.filter (fun (a, _) -> a <> addr) heap.objects

(** 获取对象数据地址 *)
let obj_data_addr addr = addr + 8

(** 标记阶段：从根对象开始递归标记 *)
let mark heap roots =
  let rec mark_obj addr =
    match List.assoc_opt addr heap.objects with
    | Some header ->
        if not header.marked then begin
          header.marked <- true;
          (* 根据对象类型递归标记引用 *)
          match header.tag with
          | t when t = tag_list || t = tag_tuple || t = tag_array ->
              (* 标记列表/元组/数组中的每个元素 *)
              let count = header.size / 4 in
              for i = 0 to count - 1 do
                let elem_addr = Bytes.get_int32_le heap.memory (obj_data_addr addr + i * 4) in
                if elem_addr <> 0l then
                  mark_obj (Int32.to_int elem_addr)
              done
          | t when t = tag_closure || t = tag_ctor || t = tag_record ->
              (* 标记环境引用 *)
              let env_addr = Bytes.get_int32_le heap.memory (obj_data_addr addr) in
              if env_addr <> 0l then
                mark_obj (Int32.to_int env_addr)
          | t when t = tag_ref ->
              (* 标记引用指向的对象 *)
              let ref_addr = Bytes.get_int32_le heap.memory (obj_data_addr addr) in
              if ref_addr <> 0l then
                mark_obj (Int32.to_int ref_addr)
          | _ -> ()
        end
    | None -> ()
  in
  List.iter mark_obj roots

(** 清除阶段：回收未标记的对象 *)
let sweep heap =
  let rec aux acc = function
    | [] -> acc
    | (addr, header) :: rest ->
        if header.marked then begin
          header.marked <- false;  (* 清除标记，为下一次 GC 做准备 *)
          aux ((addr, header) :: acc) rest
        end else begin
          (* 回收内存 - 简单实现：不合并空闲块 *)
          heap.allocated <- heap.allocated - header.size;
          aux acc rest
        end
  in
  heap.objects <- aux [] heap.objects

(** 运行完整的 GC 周期 *)
let run_gc heap roots =
  mark heap roots;
  sweep heap

(** 获取堆统计信息 *)
let heap_stats heap =
  let total_objects = List.length heap.objects in
  let total_bytes = heap.allocated in
  (total_objects, total_bytes, heap.heap_size)

(** 字符串表示 *)
let string_of_heap_stats heap =
  let (objects, bytes, capacity) = heap_stats heap in
  Printf.sprintf "Heap: %d objects, %d/%d bytes (%d%%)"
    objects bytes capacity (bytes * 100 / capacity)
