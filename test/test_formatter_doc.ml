open Core
open My_lang

let test_format_simple () =
  let e = Ast.EAdd (Ast.EInt 1, Ast.EInt 2) in
  let s = Formatter.format e in
  if String.equal s "1 + 2" then
    printf "[PASS] test_format_simple\n"
  else
    printf "[FAIL] test_format_simple: expected '1 + 2', got '%s'\n" s

let test_format_let () =
  let e = Ast.ELet ("x", Ast.EInt 42, Ast.EVar "x") in
  let s = Formatter.format e in
  if String.equal s "let x = 42 in x" then
    printf "[PASS] test_format_let\n"
  else
    printf "[FAIL] test_format_let: expected 'let x = 42 in x', got '%s'\n" s

let test_format_fun () =
  let e = Ast.EFun ("x", Ast.EAdd (Ast.EVar "x", Ast.EInt 1)) in
  let s = Formatter.format e in
  if String.equal s "fun x ->\n  x + 1" then
    printf "[PASS] test_format_fun\n"
  else
    printf "[FAIL] test_format_fun: expected 'fun x ->\\n  x + 1', got '%s'\n" s

let test_format_list () =
  let e = Ast.EList [Ast.EInt 1; Ast.EInt 2; Ast.EInt 3] in
  let s = Formatter.format e in
  if String.equal s "[1; 2; 3]" then
    printf "[PASS] test_format_list\n"
  else
    printf "[FAIL] test_format_list: expected '[1; 2; 3]', got '%s'\n" s

let test_doc_generation () =
  let e = Ast.ELet ("add", Ast.EFun ("x", Ast.EFun ("y", Ast.EAdd (Ast.EVar "x", Ast.EVar "y"))),
    Ast.ETypeDef ("color", [], [("Red", None, None); ("Green", None, None); ("Blue", None, None)])) in
  let doc = Doc_generator.generate_module_doc e in
  if List.length doc.items >= 2 then
    printf "[PASS] test_doc_generation: generated %d items\n" (List.length doc.items)
  else
    printf "[FAIL] test_doc_generation: expected at least 2 items\n"

let test_doc_markdown () =
  let e = Ast.ELet ("x", Ast.EInt 42, Ast.EVar "x") in
  let md = Doc_generator.generate_markdown_file "test" e in
  if String.is_substring md ~substring:"x" then
    printf "[PASS] test_doc_markdown\n"
  else
    printf "[FAIL] test_doc_markdown: expected 'x' in markdown\n"

let () =
  test_format_simple ();
  test_format_let ();
  test_format_fun ();
  test_format_list ();
  test_doc_generation ();
  test_doc_markdown ();
  printf "\nFormatter and doc generator tests completed.\n"
