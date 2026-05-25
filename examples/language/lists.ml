(* 列表操作 *)
let xs = [1, 2, 3, 4, 5] in
let ys = map (fun x -> x * 2) xs in
let zs = filter (fun x -> x > 2) xs in
let total = fold (fun acc -> fun x -> acc + x) 0 xs in
(show ys, show zs, total)
(* => ("[2; 4; 6; 8; 10]", "[3; 4; 5]", 15) *)
