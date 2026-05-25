(* Result 类型 *)
type int_result = Ok of int | Error of string;

(* 测试 Ok *)
match Ok 42 with
| Ok x -> x
| Error _ -> 0
