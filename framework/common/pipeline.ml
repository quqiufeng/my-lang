(** 统一的编译执行管线

    基于 Language 接口，提供通用的 parse -> typecheck -> compile/execute 管线。
*)

open Language_intf

(** 创建统一执行管线 *)
module Make (L : Language) = struct
  open L
  
  (** 完整的解释执行：源码 -> AST -> 类型检查 -> 求值 *)
  let run source =
    try
      let ast = Frontend.parse source in
      let _ = TypeSystem.typecheck ast in
      let value = Evaluator.eval ast in
      Ok value
    with
    | Pos.SyntaxError msg -> Error ("Syntax error: " ^ msg)
    | Pos.ParseError msg -> Error ("Parse error: " ^ msg)
    | Pos.TypeError msg -> Error ("Type error: " ^ msg)
    | Pos.RuntimeError (msg, pos) ->
        let pos_str = match pos with
          | Some p -> " at " ^ Pos.string_of_pos p
          | None -> ""
        in
        Error ("Runtime error" ^ pos_str ^ ": " ^ msg)
    | exn -> Error ("Error: " ^ Printexc.to_string exn)
  
  (** 从文件执行 *)
  let run_file filename =
    try
      let source = Core.In_channel.read_all filename in
      run source
    with Sys_error msg -> Error ("Cannot read file: " ^ msg)
  
  (** 编译执行：源码 -> AST -> 类型检查 -> 字节码 -> 执行 *)
  let compile_and_run source =
    try
      let ast = Frontend.parse source in
      let _ = TypeSystem.typecheck ast in
      let bytecode = Compiler.compile ast in
      let vm_value = Compiler.execute bytecode in
      Ok vm_value
    with
    | Pos.SyntaxError msg -> Error ("Syntax error: " ^ msg)
    | Pos.ParseError msg -> Error ("Parse error: " ^ msg)
    | Pos.TypeError msg -> Error ("Type error: " ^ msg)
    | exn -> Error ("Compile error: " ^ Printexc.to_string exn)
  
  (** 仅编译为字节码 *)
  let compile_only source =
    try
      let ast = Frontend.parse source in
      let _ = TypeSystem.typecheck ast in
      let bytecode = Compiler.compile ast in
      Ok bytecode
    with
    | Pos.SyntaxError msg -> Error ("Syntax error: " ^ msg)
    | Pos.ParseError msg -> Error ("Parse error: " ^ msg)
    | Pos.TypeError msg -> Error ("Type error: " ^ msg)
    | exn -> Error ("Compile error: " ^ Printexc.to_string exn)
  
  (** 仅类型检查 *)
  let typecheck_only source =
    try
      let ast = Frontend.parse source in
      let typ = TypeSystem.typecheck ast in
      Ok typ
    with
    | Pos.SyntaxError msg -> Error ("Syntax error: " ^ msg)
    | Pos.ParseError msg -> Error ("Parse error: " ^ msg)
    | Pos.TypeError msg -> Error ("Type error: " ^ msg)
    | exn -> Error ("Error: " ^ Printexc.to_string exn)
end
