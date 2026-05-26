open Core
open My_lang
open My_lang.Ast

let test_constant_folding_arithmetic () =
  let e = EAdd (EInt 1, EInt 2) in
  let e' = Optimizer.optimize e in
  match e' with
  | EInt 3 -> printf "[PASS] test_constant_folding_arithmetic\n"
  | _ -> printf "[FAIL] test_constant_folding_arithmetic: expected EInt 3\n"

let test_constant_folding_comparison () =
  let e = EEq (EInt 1, EInt 1) in
  let e' = Optimizer.optimize e in
  match e' with
  | EBool true -> printf "[PASS] test_constant_folding_comparison\n"
  | _ -> printf "[FAIL] test_constant_folding_comparison: expected EBool true\n"

let test_constant_folding_logic () =
  let e = EAnd (EBool true, EBool false) in
  let e' = Optimizer.optimize e in
  match e' with
  | EBool false -> printf "[PASS] test_constant_folding_logic\n"
  | _ -> printf "[FAIL] test_constant_folding_logic: expected EBool false\n"

let test_dead_code_elimination_if () =
  let e = EIf (EBool true, EInt 42, EInt 0) in
  let e' = Optimizer.optimize e in
  match e' with
  | EInt 42 -> printf "[PASS] test_dead_code_elimination_if\n"
  | _ -> printf "[FAIL] test_dead_code_elimination_if: expected EInt 42\n"

let test_dead_code_elimination_while () =
  let e = EWhile (EBool false, EInt 1) in
  let e' = Optimizer.optimize e in
  match e' with
  | ETuple [] -> printf "[PASS] test_dead_code_elimination_while\n"
  | _ -> printf "[FAIL] test_dead_code_elimination_while: expected ETuple []\n"

let test_nested_optimization () =
  let e = EAdd (EMul (EInt 2, EInt 3), EInt 4) in
  let e' = Optimizer.optimize e in
  match e' with
  | EInt 10 -> printf "[PASS] test_nested_optimization\n"
  | _ -> printf "[FAIL] test_nested_optimization: expected EInt 10\n"

let test_short_circuit () =
  let e = EAnd (EBool false, EVar "x") in
  let e' = Optimizer.optimize e in
  match e' with
  | EBool false -> printf "[PASS] test_short_circuit\n"
  | _ -> printf "[FAIL] test_short_circuit: expected EBool false\n"

let test_no_optimization_needed () =
  let e = EAdd (EVar "x", EInt 1) in
  let e' = Optimizer.optimize e in
  match e' with
  | EAdd (EVar "x", EInt 1) -> printf "[PASS] test_no_optimization_needed\n"
  | _ -> printf "[FAIL] test_no_optimization_needed: expression should not change\n"

let test_string_concat () =
  let e = ECat (EString "hello, ", EString "world") in
  let e' = Optimizer.optimize e in
  match e' with
  | EString "hello, world" -> printf "[PASS] test_string_concat\n"
  | _ -> printf "[FAIL] test_string_concat: expected 'hello, world'\n"

let test_let_optimization () =
  let e = ELet ("x", EAdd (EInt 1, EInt 2), EVar "x") in
  let e' = Optimizer.optimize e in
  match e' with
  | ELet ("x", EInt 3, EVar "x") -> printf "[PASS] test_let_optimization\n"
  | _ -> printf "[FAIL] test_let_optimization: expected let x = 3 in x\n"

let () =
  test_constant_folding_arithmetic ();
  test_constant_folding_comparison ();
  test_constant_folding_logic ();
  test_dead_code_elimination_if ();
  test_dead_code_elimination_while ();
  test_nested_optimization ();
  test_short_circuit ();
  test_no_optimization_needed ();
  test_string_concat ();
  test_let_optimization ();
  printf "\nOptimizer tests completed.\n"
