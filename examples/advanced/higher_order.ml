(* 高阶函数示例 *)

(* 组合函数 *)
let compose = fun f -> fun g -> fun x -> f (g x)
in

(* 管道操作 *)
let pipe = fun x -> fun f -> f x
in

(* 部分应用 *)
let add = fun x -> fun y -> x + y
in
let add5 = add 5
in

(* 使用 *)
let result = add5 10
in
result
(* => 15 *)
