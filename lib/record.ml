(** 记录模块：提供高效的记录字段操作 *)

open Ast

(** 字符串 Map 模块 *)
module StringMap = Map.Make(String)

(** 记录类型：使用关联列表保持兼容 *)
type t = (string * value ref) list

(** 空记录 *)
let empty : t = []

(** 从关联列表创建记录 *)
let of_list lst : t = lst

(** 转换为关联列表 *)
let to_list (record : t) = record

(** 查找字段 - O(n) *)
let lookup (record : t) field =
  match List.assoc_opt field record with
  | Some r -> Some r
  | None -> None

(** 设置字段值 - O(n) *)
let set (record : t) field value =
  match List.assoc_opt field record with
  | Some r -> r := value; true
  | None -> false

(** 添加字段 - O(1) *)
let add (record : t) field value : t =
  (field, ref value) :: record

(** 检查字段是否存在 - O(n) *)
let mem (record : t) field =
  List.mem_assoc field record

(** 合并两个记录，右边优先 *)
let merge (left : t) (right : t) : t =
  let merged = List.map (fun (k, r) ->
    match List.assoc_opt k right with
    | Some new_r -> (k, new_r)
    | None -> (k, r)
  ) left in
  let added = List.filter (fun (k, _) -> not (List.mem_assoc k left)) right in
  merged @ added

(** 更新记录 *)
let update (record : t) fields : t =
  let new_fields = List.map (fun (k, v) -> (k, ref v)) fields in
  merge record new_fields

(** 使用 Map 进行批量查找优化 *)
let with_map (record : t) f =
  let map = List.fold_left (fun acc (k, r) -> StringMap.add k r acc) StringMap.empty record in
  f map

(** 从 Map 中查找 - O(log n) *)
let lookup_in_map map field =
  StringMap.find_opt field map

(** 转换为值列表（用于显示） *)
let to_value_list (record : t) =
  List.map (fun (k, r) -> (k, !r)) record
