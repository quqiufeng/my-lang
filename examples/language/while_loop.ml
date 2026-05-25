(* while 循环 *)
let i = 0 in
let sum = 0 in
while i <= 10 do
  let sum = sum + i in
  let i = i + 1 in
  ()
done;
sum
(* => 55 *)
