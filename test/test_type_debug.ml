open Core
open My_lang

let () =
  print_endline "=== Type Debug ===";
  
  (* 测试 1: ESeq with module *)
  let code1 = "module M = struct 42 end; M.__value" in
  (try
    let expr1 = My_lang.parse code1 in
    Printf.printf "AST: %s\n" (match expr1 with
      | Ast.ESeq (e1, e2) -> "ESeq(_, _)"
      | _ -> "other");
    let t = My_lang.typecheck expr1 in
    Printf.printf "Type of '%s': %s\n" code1 (Types.string_of_type t)
  with
  | e -> Printf.printf "Error: %s\n" (Exn.to_string e));
