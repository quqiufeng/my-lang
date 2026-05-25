(* Option 类型 *)
type int_option = None | Some of int;

(* 测试 Some *)
match Some 42 with
| Some x -> x
| None -> 0
