(** 基础语言模板 - CLI 入口 *)

open My_language

let () =
  if Array.length Sys.argv > 1 then
    (* 运行文件 *)
    let filename = Sys.argv.(1) in
    let source =
      let ic = open_in filename in
      let n = in_channel_length ic in
      let s = really_input_string ic n in
      close_in ic;
      s
    in
    match Eval.run source with
    | Ok v -> print_endline (Ast.string_of_value v)
    | Error msg ->
        prerr_endline msg;
        exit 1
  else
    (* REPL *)
    let () = print_endline "MyLanguage v0.1.0" in
    let () = print_endline "Type :quit to exit" in
    let rec loop () =
      print_string "> ";
      flush stdout;
      match input_line stdin with
      | exception End_of_file -> ()
      | ":quit" | ":q" -> print_endline "Bye!"
      | line ->
          (match Eval.run line with
           | Ok v -> print_endline (Ast.string_of_value v)
           | Error msg -> prerr_endline ("Error: " ^ msg));
          loop ()
    in
    loop ()
