open Core
open My_lang

let () =
  print_endline "=== Parser Dot Test ===";
  
  (* 测试 1: 简单的点号访问 *)
  let code1 = "M.x" in
  (try
    let expr1 = My_lang.parse code1 in
    print_endline ("Parsed: " ^ code1);
    match expr1 with
    | Ast.EDot (e, f) -> 
        Printf.printf "EDot(%s, %s)\n" 
          (match e with Ast.EVar v -> "EVar " ^ v | _ -> "other") f
    | Ast.EVar s -> Printf.printf "EVar(%s)\n" s
    | Ast.ERecordGet (e, f) -> Printf.printf "ERecordGet(other, %s)\n" f
    | _ -> print_endline "Wrong: other"
  with
  | e -> Printf.printf "Error parsing '%s': %s\n" code1 (Exn.to_string e));
