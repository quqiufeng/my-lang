(** 高性能数组/列表操作

    提供 O(1) 索引和安全的数组操作，替代 List.nth 和重复的 List.length。
*)

open Core

(** 安全的列表索引，单次遍历，避免 List.length + List.nth 的组合 *)
let list_nth_opt lst idx =
  if idx < 0 then None
  else
    let rec loop i = function
      | [] -> None
      | x :: _ when i = idx -> Some x
      | _ :: xs -> loop (i + 1) xs
    in
    loop 0 lst

(** 获取列表长度（如果已知则直接返回） *)
let list_length lst =
  let rec loop acc = function
    | [] -> acc
    | _ :: xs -> loop (acc + 1) xs
  in
  loop 0 lst

(** 带长度缓存的列表包装 *)
type 'a sized_list = {
  lst : 'a list;
  mutable len : int option;
}

let make_sized lst = { lst; len = None }

let sized_length sl =
  match sl.len with
  | Some n -> n
  | None ->
      let n = list_length sl.lst in
      sl.len <- Some n;
      n

let sized_nth sl idx = list_nth_opt sl.lst idx

(** 数组安全索引 *)
let array_get_opt arr idx =
  if idx >= 0 && idx < Array.length arr
  then Some arr.(idx)
  else None

(** 数组安全设置 *)
let array_set_opt arr idx v =
  if idx >= 0 && idx < Array.length arr
  then (arr.(idx) <- v; true)
  else false

(** 将列表分批处理，避免大列表的栈溢出 *)
let rec list_iter_batch ~batch_size f lst =
  let rec process_chunk acc remaining count =
    match remaining with
    | _ when count <= 0 -> (List.rev acc, remaining)
    | [] -> (List.rev acc, remaining)
    | x :: xs -> process_chunk (f x :: acc) xs (count - 1)
  in
  match lst with
  | [] -> ()
  | _ ->
      let _, rest = process_chunk [] lst batch_size in
      list_iter_batch ~batch_size f rest

(** 高效的列表反转追加：rev_append 替代 a @ b *)
let list_append_fast a b = List.rev_append (List.rev a) b
