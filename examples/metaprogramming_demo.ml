(** 元编程示例：展示宏和 Quote 的用法

    这个示例演示如何使用底座提供的元编程能力。
*)

open Framework.Ast.Ast_types
open Framework.Metaprogramming

let () =
  print_endline "=== 元编程示例 ===\n";
  
  (* 示例 1: 常量折叠 *)
  print_endline "1. 常量折叠:";
  let expr1 = EBinary (Add, ELit (LInt 1), ELit (LInt 2)) in
  print_endline ("  原始: " ^ Ast_types.string_of_expr expr1);
  let folded = Ctfe.constant_fold expr1 in
  print_endline ("  折叠后: " ^ Ast_types.string_of_expr folded);
  
  (* 示例 2: Quote *)
  print_endline "\n2. Quote 机制:";
  let expr2 = EBinary (Add, ELit (LInt 10), ELit (LInt 20)) in
  let quoted = Quote.quote_expr expr2 in
  print_endline ("  原始表达式: " ^ Ast_types.string_of_expr expr2);
  print_endline ("  Quote 后: " ^ Ast_types.string_of_expr quoted);
  
  (* 示例 3: 宏展开 *)
  print_endline "\n3. 宏展开:";
  let macro_env = Macro.builtin_macros () in
  
  (* unless 宏: unless (x = 0) (print "not zero") *)
  let unless_call = EMacro ("unless", [
    EBinary (Eq, EVar "x", ELit (LInt 0));
    EApp (EVar "print", [ELit (LString "not zero")])
  ]) in
  print_endline ("  宏调用: " ^ Ast_types.string_of_expr unless_call);
  let expanded = Macro.expand_macros macro_env unless_call in
  print_endline ("  展开后: " ^ Ast_types.string_of_expr expanded);
  
  (* 示例 4: 嵌套宏 *)
  print_endline "\n4. 嵌套宏:";
  let nested = EMacro ("when", [
    EBinary (Gt, EVar "x", ELit (LInt 0));
    EMacro ("unless", [
      EBinary (Eq, EVar "x", ELit (LInt 1));
      EApp (EVar "print", [ELit (LString "not one")])
    ])
  ]) in
  print_endline ("  嵌套宏: " ^ Ast_types.string_of_expr nested);
  let expanded_nested = Macro.expand_macros macro_env nested in
  print_endline ("  展开后: " ^ Ast_types.string_of_expr expanded_nested);
  
  (* 示例 5: 复杂常量折叠 *)
  print_endline "\n5. 复杂常量折叠:";
  let complex = EBinary (Add,
    EBinary (Mul, ELit (LInt 2), ELit (LInt 3)),
    EBinary (Sub, ELit (LInt 10), ELit (LInt 4))
  ) in
  print_endline ("  原始: " ^ Ast_types.string_of_expr complex);
  let folded_complex = Ctfe.constant_fold complex in
  print_endline ("  折叠后: " ^ Ast_types.string_of_expr folded_complex);
  
  print_endline "\n=== 示例结束 ==="
