open Core
open My_lang
open My_lang.Ast

let run_test name code check =
  match My_lang.run_exn code with
  | Ok v when check v ->
      printf "[PASS] %s\n" name
  | Ok v ->
      printf "[FAIL] %s: got %s\n" name (string_of_value v)
  | Error msg ->
      printf "[FAIL] %s: unexpected error: %s\n" name msg

let () =
  run_test "gadt_type_checking"
    "type expr = | Val : int -> expr | Add : expr -> expr; let x = Val 42 in x"
    (function VCtor ("Val", Some (VInt 42)) -> true | _ -> false);

  run_test "gadt_simple_eval"
    "type expr = | Val : int -> expr | Add : expr -> expr; let rec eval = fun e -> match e with | Val x -> x | Add e1 -> eval e1 + 1 in eval (Add (Val 2))"
    (function VInt 3 -> true | _ -> false);

  run_test "gadt_type_params"
    "type 'a option = | None : 'a option | Some : 'a -> 'a option; let x = Some 42 in x"
    (function VCtor ("Some", Some (VInt 42)) -> true | _ -> false);

  run_test "gadt_nullary"
    "type 'a option = | None : 'a option | Some : 'a -> 'a option; None"
    (function VCtor ("None", None) -> true | _ -> false);

  run_test "gadt_recursive_type"
    "type 'a tree = | Leaf : 'a -> 'a tree | Node : 'a tree -> 'a tree; let t = Node (Leaf 42) in t"
    (function VCtor ("Node", Some (VCtor ("Leaf", Some (VInt 42)))) -> true | _ -> false);

  printf "GADT tests done.\n"