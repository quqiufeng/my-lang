(* 递归函数 *)
let rec factorial = fun n ->
  if n = 0 then 1 else n * factorial (n - 1)
in
let rec fibonacci = fun n ->
  if n <= 1 then n else fibonacci (n - 1) + fibonacci (n - 2)
in
(factorial 5, fibonacci 10)
(* => (120, 55) *)
