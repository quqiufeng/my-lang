(* Queue 标准库 - 基于自定义 ADT 实现
   
   避免列表字面量语法问题
*)

type 'a option = None | Some of 'a;

type 'a queue =
  | Empty
  | Enqueue of 'a * 'a queue;

(* 空队列 *)
let empty = Empty in

(* 入队 *)
let enqueue = fun x -> fun q -> Enqueue (x, q) in

(* 出队 - 使用辅助递归 *)
let rec dequeue = fun q ->
  match q with
  | Empty -> None
  | Enqueue (x, Empty) -> Some (x, Empty)
  | Enqueue (x, rest) ->
      match dequeue rest with
      | Some (y, new_rest) -> Some (y, Enqueue (x, new_rest))
      | None -> Some (x, Empty)
in

(* 查看队首 *)
let rec peek = fun q ->
  match q with
  | Empty -> None
  | Enqueue (x, Empty) -> Some x
  | Enqueue (_, rest) -> peek rest
in

(* 测试 *)
let q = empty in
let q = enqueue 1 q in
let q = enqueue 2 q in
let q = enqueue 3 q in

match dequeue q with
| Some (x, q2) -> x
| None -> 0
