open My_lang

let () =
  try
    let expr = parse "let rec factorial = fun n -> if n = 0 then 1 else n * factorial (n - 1) in factorial 5" in
    let code = compile expr in
    let result = run_bytecode code in
    Printf.printf "Result: %s\n" (Vm.string_of_vm_value result)
  with
  | Vm.VMError msg -> Printf.printf "VMError: %s\n" msg
  | exn -> Printf.printf "Exception: %s\n" (Printexc.to_string exn)
