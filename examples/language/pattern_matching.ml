(* 模式匹配 *)
let sum = fun xs ->
  match xs with
  | [] -> 0
  | h :: t -> h + sum t
in
sum [1, 2, 3, 4, 5]
(* => 15 *)
