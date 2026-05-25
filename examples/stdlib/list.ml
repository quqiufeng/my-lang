(* 列表工具函数 *)

let rec length = fun xs ->
  match xs with
  | [] -> 0
  | _ :: t -> 1 + length t
in

let rec reverse = fun xs ->
  let rec helper = fun acc -> fun ys ->
    match ys with
    | [] -> acc
    | h :: t -> helper (h :: acc) t
  in
  helper [] xs
in

let rec nth = fun n -> fun xs ->
  match xs with
  | [] -> 0
  | h :: t -> if n = 0 then h else nth (n - 1) t
in

()
