(* Stack 标准库 - 基于列表实现
   
   使用 let/in 语法
*)

type 'a option = None | Some of 'a;

type 'a stack =
  | Empty
  | Push of 'a * 'a stack;

(* 空栈 *)
let empty = Empty in

(* 入栈 *)
let push = fun x -> fun s -> Push (x, s) in

(* 出栈 *)
let pop = fun s ->
  match s with
  | Empty -> None
  | Push (x, rest) -> Some (x, rest)
in

(* 查看栈顶 *)
let peek = fun s ->
  match s with
  | Empty -> None
  | Push (x, _) -> Some x
in

(* 检查是否为空 *)
let is_empty = fun s ->
  match s with
  | Empty -> true
  | _ -> false
in

(* 测试 *)
let s = empty in
let s = push 1 s in
let s = push 2 s in
let s = push 3 s in

match pop s with
| Some (x, s2) -> x
| None -> 0
