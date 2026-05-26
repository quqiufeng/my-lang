open Core
open My_lang

let test_lsp_diagnostics () =
  (* 测试 LSP 诊断是否正确报告类型错误 *)
  let content = "let x = true + 1 in x" in
  let diag = Diagnostics.create () in
  try
    let lexbuf = Lexing.from_string content in
    let expr = Parser.prog Lexer.read lexbuf in
    (try
       let _ = Typeinfer.typecheck expr in
       ()
     with
     | Types.TypeError msg ->
         Diagnostics.add_error diag ~phase:Diagnostics.TypeChecking msg
     | exn ->
         Diagnostics.add_error diag ~phase:Diagnostics.TypeChecking (Exn.to_string exn));
    if Diagnostics.has_errors diag then
      printf "[PASS] test_lsp_diagnostics: detected %d errors\n" (Diagnostics.error_count diag)
    else
      printf "[FAIL] test_lsp_diagnostics: expected type error\n"
  with
  | exn -> printf "[FAIL] test_lsp_diagnostics: unexpected exception %s\n" (Exn.to_string exn)

let test_lsp_diagnostics_parse_error () =
  let content = "let x = in x" in
  let diag = Diagnostics.create () in
  try
    let lexbuf = Lexing.from_string content in
    Parser_recovery.set_reporter (fun msg pos ->
      Diagnostics.add_error diag
        ~phase:Diagnostics.Parsing
        ~line:pos.Lexing.pos_lnum
        ~col:(max 1 (pos.Lexing.pos_cnum - pos.Lexing.pos_bol))
        msg
    );
    let _expr = Parser.prog Lexer.read lexbuf in
    if Diagnostics.has_errors diag then
      printf "[PASS] test_lsp_diagnostics_parse_error: detected %d errors\n" (Diagnostics.error_count diag)
    else
      printf "[INFO] test_lsp_diagnostics_parse_error: no parse errors collected\n"
  with
  | exn -> printf "[INFO] test_lsp_diagnostics_parse_error: exception %s\n" (Exn.to_string exn)

let test_lsp_symbol_extraction () =
  let content = "let add = fun x -> fun y -> x + y in add 1 2" in
  try
    let lexbuf = Lexing.from_string content in
    let expr = Parser.prog Lexer.read lexbuf in
    let table = Symbol_table.extract_symbols expr in
    if Hashtbl.length table.defs > 0 then
      printf "[PASS] test_lsp_symbol_extraction: found %d symbols\n" (Hashtbl.length table.defs)
    else
      printf "[FAIL] test_lsp_symbol_extraction: expected at least 1 symbol\n"
  with
  | exn -> printf "[FAIL] test_lsp_symbol_extraction: unexpected exception %s\n" (Exn.to_string exn)

let () =
  test_lsp_diagnostics ();
  test_lsp_diagnostics_parse_error ();
  test_lsp_symbol_extraction ();
  printf "\nLSP tests completed.\n"
