open Core
open My_lang

let test_plugin_system_list () =
  let backends = Plugin_system.list_backends () in
  if List.length backends >= 3 then
    printf "[PASS] test_plugin_system_list: found %d backends\n" (List.length backends)
  else
    printf "[FAIL] test_plugin_system_list: expected at least 3 backends\n"

let test_plugin_interpreter () =
  let result = Plugin_system.compile_to "interpreter" (Ast.EInt 42) in
  if result.Plugin_system.success then
    printf "[PASS] test_plugin_interpreter: %s\n" (Option.value result.output ~default:"no output")
  else
    printf "[FAIL] test_plugin_interpreter: %s\n" (String.concat ~sep:", " result.errors)

let test_plugin_bytecode () =
  let result = Plugin_system.compile_to "bytecode" (Ast.EAdd (Ast.EInt 1, Ast.EInt 2)) in
  if result.Plugin_system.success then
    printf "[PASS] test_plugin_bytecode: %s\n" (Option.value result.output ~default:"no output")
  else
    printf "[FAIL] test_plugin_bytecode: %s\n" (String.concat ~sep:", " result.errors)

let test_plugin_unknown () =
  let result = Plugin_system.compile_to "unknown_backend" (Ast.EInt 1) in
  if not result.Plugin_system.success then
    printf "[PASS] test_plugin_unknown: correctly rejected unknown backend\n"
  else
    printf "[FAIL] test_plugin_unknown: should have failed\n"

let test_error_monad_basic () =
  let open Error_monad in
  let r = ok 42 in
  match r with
  | Ok 42 -> printf "[PASS] test_error_monad_basic\n"
  | _ -> printf "[FAIL] test_error_monad_basic\n"

let test_error_monad_bind () =
  let open Error_monad in
  let r = bind_result (ok 5) (fun x -> ok (x * 2)) in
  match r with
  | Ok 10 -> printf "[PASS] test_error_monad_bind\n"
  | _ -> printf "[FAIL] test_error_monad_bind\n"

let test_error_monad_sequence () =
  let open Error_monad in
  let results = [Ok 1; Ok 2; Ok 3] in
  match sequence results with
  | Ok [1; 2; 3] -> printf "[PASS] test_error_monad_sequence\n"
  | _ -> printf "[FAIL] test_error_monad_sequence\n"

let () =
  test_plugin_system_list ();
  test_plugin_interpreter ();
  test_plugin_bytecode ();
  test_plugin_unknown ();
  test_error_monad_basic ();
  test_error_monad_bind ();
  test_error_monad_sequence ();
  printf "\nPlugin system and error monad tests completed.\n"
