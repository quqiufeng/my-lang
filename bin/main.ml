(** MyLang - 交互式解释器 *)

open Core
open My_lang

let print_usage () =
  print_endline "MyLang - A simple functional language";
  print_endline "";
  print_endline "Usage:";
  print_endline "  my_lang           Start REPL";
  print_endline "  my_lang <file>   Run a file";
  print_endline "";
  print_endline "Examples:";
  print_endline "  1 + 2 * 3";
  print_endline "  let x = 10 in x + 5";
  print_endline "  fun x -> x + 1";
  print_endline "  let add = fun x -> fun y -> x + y in add 3 4";
  print_endline "  if true then 1 else 0"

let rec repl () =
  print_string "my-lang> ";
  Out_channel.flush stdout;
  match In_channel.input_line In_channel.stdin with
  | None | Some "exit" | Some "quit" -> print_endline "Bye!"
  | Some "" -> repl ()
  | Some line ->
      (match My_lang.run_exn line with
       | Ok v -> print_endline (My_lang.Ast.string_of_value v)
       | Error msg -> print_endline msg);
      repl ()

let run_file filename =
  let content = In_channel.read_all filename in
  match My_lang.run_exn content with
  | Ok v -> print_endline (My_lang.Ast.string_of_value v)
  | Error msg ->
      print_endline msg;
      exit 1

let () =
  match Sys.get_argv () with
  | [| _ |] ->
      print_usage ();
      print_endline "";
      repl ()
  | [| _; filename |] -> run_file filename
  | _ ->
      print_endline "Too many arguments";
      exit 1
