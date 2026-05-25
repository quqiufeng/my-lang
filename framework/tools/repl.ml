(** 通用 REPL 实现

    基于 Language 接口，提供交互式解释器。
    任何实现了 Language.S 的语言都可以直接使用此 REPL。
*)

open Core
open Language_intf

module Make (L : Language) = struct
  module Pipeline = Pipeline.Make (L)
  
  let print_usage () =
    Printf.printf "%s v%s - %s\n\n" L.name L.version L.description;
    Printf.printf "REPL 命令:\n";
    Printf.printf "  :help       显示帮助\n";
    Printf.printf "  :quit / :q  退出\n";
    Printf.printf "  :type       显示表达式类型\n";
    Printf.printf "  :ast        显示 AST\n";
    Printf.printf "  :bytecode   显示字节码\n";
    Printf.printf "\n"
  
  let show_type line =
    match Pipeline.typecheck_only line with
    | Ok typ -> Printf.printf "- : %s\n%!" (L.TypeSystem.string_of_type typ)
    | Error msg -> Printf.printf "Error: %s\n%!" msg
  
  let show_ast line =
    try
      let ast = L.Frontend.parse line in
      Printf.printf "%s\n%!" (L.Frontend.dump_ast ast)
    with exn -> Printf.printf "Error: %s\n%!" (Exn.to_string exn)
  
  let show_bytecode line =
    match Pipeline.compile_only line with
    | Ok bytecode -> Printf.printf "%s\n%!" (L.Compiler.disassemble bytecode)
    | Error msg -> Printf.printf "Error: %s\n%!" msg
  
  let eval_line line =
    match Pipeline.run line with
    | Ok value -> Printf.printf "%s\n%!" (L.Evaluator.string_of_value value)
    | Error msg -> Printf.printf "%s\n%!" msg
  
  let rec loop () =
    Printf.printf "%s> %!" (String.lowercase L.name);
    match In_channel.input_line In_channel.stdin with
    | None -> Printf.printf "\nBye!\n"
    | Some "" -> loop ()
    | Some ":help" -> print_usage (); loop ()
    | Some ":quit" | Some ":q" | Some "exit" -> Printf.printf "Bye!\n"
    | Some line when String.is_prefix line ~prefix:":type " ->
        let code = String.sub line ~pos:6 ~len:(String.length line - 6) in
        show_type code; loop ()
    | Some line when String.is_prefix line ~prefix:":ast " ->
        let code = String.sub line ~pos:5 ~len:(String.length line - 5) in
        show_ast code; loop ()
    | Some line when String.is_prefix line ~prefix:":bytecode " ->
        let code = String.sub line ~pos:10 ~len:(String.length line - 10) in
        show_bytecode code; loop ()
    | Some line ->
        eval_line line; loop ()
  
  let start () =
    print_usage ();
    loop ()
end
