(* Map 标准库 - 基于 AVL 树 (int 键)
   
   使用柯里化函数（单参数）和 let/in 语法
*)

type 'v map = Empty | Node of int * 'v * int * 'v map * 'v map;

(* 辅助函数 *)
let max = fun a -> fun b ->
  let a : int = a in
  let b : int = b in
  if a > b then a else b
in

(* 获取树高度 *)
let rec height = fun m ->
  match m with
  | Empty -> 0
  | Node (_, _, h, _, _) -> h
in

(* 创建节点 *)
let make_node = fun k -> fun v -> fun left -> fun right ->
  let h = 1 + max (height left) (height right) in
  Node (k, v, h, left, right)
in

(* 右旋 *)
let rotate_right = fun m ->
  match m with
  | Node (k, v, _, Node (lk, lv, _, ll, lr), right) ->
      make_node lk lv ll (make_node k v lr right)
  | _ -> m
in

(* 左旋 *)
let rotate_left = fun m ->
  match m with
  | Node (k, v, _, left, Node (rk, rv, _, rl, rr)) ->
      make_node rk rv (make_node k v left rl) rr
  | _ -> m
in

(* 平衡因子 *)
let balance_factor = fun m ->
  match m with
  | Empty -> 0
  | Node (_, _, _, left, right) -> height left - height right
in

(* 平衡树 *)
let balance = fun k -> fun v -> fun left -> fun right ->
  let m = make_node k v left right in
  let bf = balance_factor m in
  if bf > 1 then
    match left with
    | Node _ ->
        if balance_factor left >= 0 then
          rotate_right m
        else
          let left_prime = rotate_left left in
          rotate_right (make_node k v left_prime right)
    | _ -> m
  else if bf < -1 then
    match right with
    | Node _ ->
        if balance_factor right <= 0 then
          rotate_left m
        else
          let right_prime = rotate_right right in
          rotate_left (make_node k v left right_prime)
    | _ -> m
  else
    m
in

(* 插入键值对 *)
let rec insert = fun k -> fun v -> fun m ->
  match m with
  | Empty -> make_node k v Empty Empty
  | Node (k2, v2, _, left, right) ->
      if k = k2 then
        make_node k v left right
      else if k < k2 then
        balance k2 v2 (insert k v left) right
      else
        balance k2 v2 left (insert k v right)
in

(* 查找键 - 返回 value 或直接默认值 *)
let rec find = fun k -> fun m ->
  match m with
  | Empty -> ""
  | Node (k2, v2, _, left, right) ->
      if k = k2 then v2
      else if k < k2 then find k left
      else find k right
in

(* 测试 *)
let m = Empty in
let m = insert 10 "ten" m in
let m = insert 5 "five" m in
let m = insert 15 "fifteen" m in
let m = insert 3 "three" m in
let m = insert 7 "seven" m in

find 5 m
