open Core
open My_lang

let () =
  let tests = [
    "quicksort", "
let rec append = fun xs -> fun ys ->
  match xs with
  | [] -> ys
  | x :: xs2 -> x :: append xs2 ys
in
let rec quicksort = fun xs ->
  match xs with
  | [] -> []
  | p :: rest ->
      let rec partition = fun left -> fun right -> fun ys ->
        match ys with
        | [] -> (left, right)
        | y :: ys2 ->
            if y <= p then partition (y :: left) right ys2
            else partition left (y :: right) ys2
      in
      let parts = partition [] [] rest in
      match parts with
      | (left, right) -> append (quicksort left) (p :: quicksort right)
in
quicksort [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5]
";
    "tree", "
type tree = | Leaf : int -> tree | Node : (tree * tree) -> tree;
let rec tree_sum = fun t ->
  match t with
  | Leaf n -> n
  | Node (l, r) -> tree_sum l + tree_sum r
in
let rec build_tree = fun n ->
  if n = 0 then Leaf 1
  else Node (build_tree (n - 1), build_tree (n - 1))
in
tree_sum (build_tree 4)
";
    "gc_stress", "
let rec make_list = fun n ->
  if n = 0 then []
  else n :: make_list (n - 1)
in
let rec sum_list = fun xs ->
  match xs with
  | [] -> 0
  | x :: xs2 -> x + sum_list xs2
in
sum_list (make_list 1000)
";
  ] in
  List.iter tests ~f:(fun (name, code) ->
    printf "Testing %s...\n" name;
    match My_lang.run_exn code with
    | Ok v -> printf "  OK: %s\n" (Ast.string_of_value v)
    | Error msg -> printf "  ERR: %s\n" msg
  )
