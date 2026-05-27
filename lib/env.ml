(** 环境模块：提供高效的环境操作 *)

open Ast

(** 字符串 Map 模块 *)
module StringMap = Map.Make(String)

(** 环境类型：使用关联列表保持兼容 *)
type t = env

(** 空环境 *)
let empty : t = []

(** 从关联列表创建环境 *)
let of_list lst : t = lst

(** 转换为关联列表 *)
let to_list (env : t) = env

(** 查找变量 - O(n) *)
let lookup (env : t) x =
  match List.assoc_opt x env with
  | Some v -> Ok v
  | None -> Error ("未绑定变量: " ^ x)

(** 添加绑定 - O(1) *)
let add (env : t) x v : t =
  (x, v) :: env

(** 批量添加绑定 *)
let add_list (env : t) lst : t =
  lst @ env

(** 检查变量是否存在 - O(n) *)
let mem (env : t) x =
  List.mem_assoc x env

(** 合并两个环境，右边优先 *)
let merge (left : t) (right : t) : t =
  right @ left

(** 映射函数 *)
let map f (env : t) : t =
  List.map (fun (k, v) -> (k, f v)) env

(** 折叠函数 *)
let fold f (env : t) acc =
  List.fold_left (fun acc (k, v) -> f k v acc) acc env

(** 使用 Map 进行批量查找优化 *)
let with_map (env : t) f =
  let map = List.fold_left (fun acc (k, v) -> StringMap.add k v acc) StringMap.empty env in
  f map

(** 从 Map 中查找 - O(log n) *)
let lookup_in_map map x =
  match StringMap.find_opt x map with
  | Some v -> Ok v
  | None -> Error ("未绑定变量: " ^ x)
