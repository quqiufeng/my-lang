(* 排序算法 - 快速排序 *)

let rec qsort = fun xs ->
  match xs with
  | [] -> []
  | pivot :: rest ->
      let lesser = filter (fun x -> x < pivot) rest in
      let greater = filter (fun x -> x >= pivot) rest in
      qsort lesser @ [pivot] @ qsort greater
in

qsort [3, 1, 4, 1, 5, 9, 2, 6]
(* => [1, 1, 2, 3, 4, 5, 6, 9] *)
