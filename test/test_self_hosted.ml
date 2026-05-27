open Core
open My_lang

let run_test name code check =
  match My_lang.run_exn code with
  | Ok v when check v ->
      printf "[PASS] %s\n" name
  | Ok v ->
      printf "[FAIL] %s: got %s\n" name (Ast.string_of_value v)
  | Error msg ->
      printf "[FAIL] %s: %s\n" name msg

let () =
  (* 测试1: 直接导入并调用 tokenize *)
  run_test "import_tokenize_simple"
    "import \"self_hosted/lexer.lang\"; tokenize \"1+2\""
    (function
      | Ast.VList [Ast.VCtor ("TInt", Some (Ast.VInt 1));
                   Ast.VCtor ("TPlus", None);
                   Ast.VCtor ("TInt", Some (Ast.VInt 2))] -> true
      | _ -> false);

  (* 测试2: 带空格 *)
  run_test "import_tokenize_with_spaces"
    "import \"self_hosted/lexer.lang\"; tokenize \"12 + 34 * 5\""
    (function
      | Ast.VList [Ast.VCtor ("TInt", Some (Ast.VInt 12));
                   Ast.VCtor ("TPlus", None);
                   Ast.VCtor ("TInt", Some (Ast.VInt 34));
                   Ast.VCtor ("TMul", None);
                   Ast.VCtor ("TInt", Some (Ast.VInt 5))] -> true
      | _ -> false);

  (* 测试3: 括号 *)
  run_test "import_tokenize_parens"
    "import \"self_hosted/lexer.lang\"; tokenize \"(1+2)\""
    (function
      | Ast.VList [Ast.VCtor ("TLparen", None);
                   Ast.VCtor ("TInt", Some (Ast.VInt 1));
                   Ast.VCtor ("TPlus", None);
                   Ast.VCtor ("TInt", Some (Ast.VInt 2));
                   Ast.VCtor ("TRparen", None)] -> true
      | _ -> false);

  (* 测试4: 负数/减法 *)
  run_test "import_tokenize_minus"
    "import \"self_hosted/lexer.lang\"; tokenize \"10-3\""
    (function
      | Ast.VList [Ast.VCtor ("TInt", Some (Ast.VInt 10));
                   Ast.VCtor ("TMinus", None);
                   Ast.VCtor ("TInt", Some (Ast.VInt 3))] -> true
      | _ -> false);

  printf "\nSelf-hosted compiler (lexer) tests completed.\n"
