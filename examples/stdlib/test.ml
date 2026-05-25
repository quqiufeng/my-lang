(* Option 类型 *)
type int_option = None | Some of int;

(* Result 类型 *)
type int_result = Ok of int | Error of string;

(* 测试 Option Some *)
assert (match Some 42 with | Some x -> x = 42 | None -> false);

(* 测试 Option None *)
assert (match None with | Some _ -> false | None -> true);

(* 测试 Result Ok *)
assert (match Ok 42 with | Ok x -> x = 42 | Error _ -> false);

(* 测试 Result Error *)
assert (match Error "fail" with | Ok _ -> false | Error _ -> true);

print "Stdlib tests passed!"
