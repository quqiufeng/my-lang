(** 内置函数环境 *)

open Ast
open Eval_helpers

(** 求值上下文，用于处理循环依赖 *)
type eval_context = {
  eval_fn: env -> expr -> (value * env, string) Result.t;
  apply_fn: env -> value -> value -> (value * env, string) Result.t;
}

(** 创建内置环境 *)
let create_builtin_env ctx =
  let import_func env v =
    match v with
    | VString filename ->
        (try
           let content = Core.In_channel.read_all filename in
           let lexbuf = Lexing.from_string content in
           let expr = Parser.prog Lexer.read lexbuf in
           let* (_, env') = ctx.eval_fn env expr in
           Ok (VUnit, env')
         with Sys_error msg -> Error ("无法导入文件: " ^ msg))
    | _ -> Error "import: 需要字符串文件名"
  in
  [ ( "head",
      VBuiltin
        ( "head",
          fun env -> function
          | VList (h :: _) -> Ok (h, env)
          | VList [] -> Error "head: 空列表"
          | _ -> Error "head: 需要列表" ) )
  ; ( "tail",
      VBuiltin
        ( "tail",
          fun env -> function
          | VList (_ :: t) -> Ok (VList t, env)
          | VList [] -> Error "tail: 空列表"
          | _ -> Error "tail: 需要列表" ) )
  ; ( "length",
      VBuiltin
        ( "length",
          fun env -> function
          | VList l -> Ok (VInt (List.length l), env)
          | VString s -> Ok (VInt (String.length s), env)
          | _ -> Error "length: 需要列表或字符串" ) )
  ; ( "print",
      VBuiltin
        ( "print",
          fun env v ->
            print_endline (string_of_value v);
            Ok (VUnit, env) ) )
  ; ( "import",
      VBuiltin
        ( "import",
          import_func ) )
  ; ( "show",
      VBuiltin
        ( "show",
          fun env v ->
            Ok (VString (string_of_value v), env) ) )
  ; ( "string_length",
      VBuiltin
        ( "string_length",
          fun env -> function
          | VString s -> Ok (VInt (String.length s), env)
          | v -> Error ("string_length: 需要字符串，但得到 " ^ type_of_value v) ) )
  ; ( "string_get",
      VBuiltin
        ( "string_get",
          fun env s ->
            Ok (VBuiltin
               ( "string_get'",
                 fun env idx ->
                   match s, idx with
                   | VString s, VInt i when i >= 0 && i < String.length s ->
                       Ok (VChar s.[i], env)
                   | VString _, VInt i ->
                       Error ("string_get: 索引越界: " ^ string_of_int i)
                   | VString _, v ->
                       Error ("string_get: 索引需要整数，但得到 " ^ type_of_value v)
                   | v, _ ->
                       Error ("string_get: 需要字符串，但得到 " ^ type_of_value v) ),
             env) ) )
  ; ( "string_sub",
      VBuiltin
        ( "string_sub",
          fun env s ->
            Ok (VBuiltin
               ( "string_sub'",
                 fun env start ->
                   Ok (VBuiltin
                      ( "string_sub''",
                        fun env len ->
                          match s, start, len with
                          | VString s, VInt start, VInt len when start >= 0 && len >= 0 && start + len <= String.length s ->
                              Ok (VString (String.sub s start len), env)
                          | VString _, VInt _, VInt _ ->
                              Error "string_sub: 索引越界"
                          | VString _, VInt _, v ->
                              Error ("string_sub: 长度需要整数，但得到 " ^ type_of_value v)
                          | VString _, v, _ ->
                              Error ("string_sub: 起始需要整数，但得到 " ^ type_of_value v)
                          | v, _, _ ->
                              Error ("string_sub: 需要字符串，但得到 " ^ type_of_value v) ),
                    env) ),
             env) ) )
  ; ( "read_file",
      VBuiltin
        ( "read_file",
          fun env -> function
          | VString filename ->
              (try
                 let content = Core.In_channel.read_all filename in
                 Ok (VString content, env)
               with Sys_error msg -> Error ("无法读取文件: " ^ msg))
          | v -> Error ("read_file: 需要字符串文件名，但得到 " ^ type_of_value v) ) )
  ; ( "write_file",
      VBuiltin
        ( "write_file",
          fun env filename ->
            Ok (VBuiltin
               ( "write_file'",
                 fun env content ->
                   match filename, content with
                   | VString filename, VString content ->
                       (try
                          Core.Out_channel.write_all filename ~data:content;
                          Ok (VUnit, env)
                        with Sys_error msg -> Error ("无法写入文件: " ^ msg))
                   | VString _, v ->
                       Error ("write_file: 内容需要字符串，但得到 " ^ type_of_value v)
                   | v, _ ->
                       Error ("write_file: 文件名需要字符串，但得到 " ^ type_of_value v) ),
             env) ) )
  ; ( "read_line",
      VBuiltin
        ( "read_line",
          fun env -> function
          | VUnit ->
              let line =
                try input_line stdin
                with End_of_file -> ""
              in
              Ok (VString line, env)
          | v -> Error ("read_line: 需要 unit，但得到 " ^ type_of_value v) ) )
  ; ( "print_string",
      VBuiltin
        ( "print_string",
          fun env -> function
          | VString s ->
              print_string s;
              Ok (VUnit, env)
          | v -> Error ("print_string: 需要字符串，但得到 " ^ type_of_value v) ) )
  ; ( "map",
      VBuiltin
        ( "map",
          fun env f ->
            Ok (VBuiltin
               ( "map'",
                 fun env xs ->
                   match xs with
                   | VList items ->
                       let rec map_items = function
                         | [] -> Ok []
                         | item :: rest ->
                             let* (v, _) = ctx.apply_fn env f item in
                             let* vs' = map_items rest in
                             Ok (v :: vs')
                       in
                       let* results = map_items items in
                       Ok (VList results, env)
                   | v -> Error ("map: 第二个参数必须是列表，但得到 " ^ type_of_value v) ),
             env)
        ) )
  ; ( "filter",
      VBuiltin
        ( "filter",
          fun env f ->
            Ok (VBuiltin
               ( "filter'",
                 fun env xs ->
                   match xs with
                   | VList items ->
                       let rec filter_items = function
                         | [] -> Ok []
                         | item :: rest ->
                             let* (v, _) = ctx.apply_fn env f item in
                             let* rest' = filter_items rest in
                             (match v with
                              | VBool b -> Ok (if b then item :: rest' else rest')
                              | v -> Error ("filter: 谓词函数必须返回布尔值，但得到 " ^ type_of_value v))
                       in
                       let* results = filter_items items in
                       Ok (VList results, env)
                   | v -> Error ("filter: 第二个参数必须是列表，但得到 " ^ type_of_value v) ),
             env)
        ) )
  ; ( "fold",
      VBuiltin
        ( "fold",
          fun env f ->
            Ok (VBuiltin
               ( "fold'",
                 fun env acc ->
                   Ok (VBuiltin
                      ( "fold''",
                        fun env xs ->
                          match xs with
                          | VList items ->
                              let rec fold_items acc = function
                                | [] -> Ok acc
                                | item :: rest ->
                                    let* (f_acc, _) = ctx.apply_fn env f acc in
                                    (match f_acc with
                                     | VFun _ | VBuiltin _ ->
                                         let* (v, _) = ctx.apply_fn env f_acc item in
                                         fold_items v rest
                                     | v -> Error ("fold: folding 函数必须接受两个参数，但得到 " ^ type_of_value v))
                              in
                              let* result = fold_items acc items in
                              Ok (result, env)
                           | v -> Error ("fold: 第三个参数必须是列表，但得到 " ^ type_of_value v) ),
                      env) ),
             env) ) )
  ; ( "range",
      VBuiltin
        ( "range",
          fun env start ->
            Ok (VBuiltin
               ( "range'",
                 fun env end_val ->
                   match start, end_val with
                   | VInt s, VInt e ->
                       let rec build_range i acc =
                         if i > e then List.rev acc
                         else build_range (i + 1) (VInt i :: acc)
                       in
                       let nums = build_range s [] in
                       Ok (VList nums, env)
                   | _ -> Error "range: 需要整数参数" ),
             env) ) )
  ; ( "sum",
      VBuiltin
        ( "sum",
          fun env xs ->
            match xs with
            | VList items ->
                let rec sum_items acc = function
                  | [] -> Ok acc
                  | VInt n :: rest -> sum_items (acc + n) rest
                  | _ -> Error "sum: 列表元素必须是整数"
                in
                let* total = sum_items 0 items in
                Ok (VInt total, env)
            | _ -> Error "sum: 需要列表" ) )
  ; ( "reverse",
      VBuiltin
        ( "reverse",
          fun env xs ->
            match xs with
            | VList items -> Ok (VList (List.rev items), env)
            | _ -> Error "reverse: 需要列表" ) )
  ; ( "append",
      VBuiltin
        ( "append",
          fun env xs ->
            Ok (VBuiltin
               ( "append'",
                 fun env ys ->
                   match xs, ys with
                   | VList a, VList b -> Ok (VList (a @ b), env)
                   | _ -> Error "append: 需要两个列表" ),
             env) ) )
  ; ( "timeit",
      VBuiltin
        ( "timeit",
          fun env f ->
            match f with
            | VFun _ | VBuiltin _ ->
                let start = Core.Time_float.now () in
                let* (result, _) = ctx.apply_fn env f (VTuple []) in
                let elapsed = Core.Time_float.diff (Core.Time_float.now ()) start in
                let ms = Core.Time_float.Span.to_ms elapsed in
                Printf.printf "[timeit] %.4f ms\n%!" ms;
                Ok (result, env)
            | _ -> Error "timeit: 需要函数" ) )
  ; ( "string_trim",
      VBuiltin
        ( "string_trim",
          fun env -> function
          | VString s -> Ok (VString (String.trim s), env)
          | v -> Error ("string_trim: 需要字符串，但得到 " ^ type_of_value v) ) )
  ; ( "string_uppercase",
      VBuiltin
        ( "string_uppercase",
          fun env -> function
          | VString s -> Ok (VString (String.uppercase_ascii s), env)
          | v -> Error ("string_uppercase: 需要字符串，但得到 " ^ type_of_value v) ) )
  ; ( "string_lowercase",
      VBuiltin
        ( "string_lowercase",
          fun env -> function
          | VString s -> Ok (VString (String.lowercase_ascii s), env)
          | v -> Error ("string_lowercase: 需要字符串，但得到 " ^ type_of_value v) ) )
  ; ( "string_concat",
      VBuiltin
        ( "string_concat",
          fun env -> function
          | VTuple [VString sep; VList items] ->
              let rec extract_strings = function
                | [] -> Ok []
                | VString s :: rest ->
                    let* rest' = extract_strings rest in
                    Ok (s :: rest')
                | _ -> Error "string_concat: 列表元素必须是字符串"
              in
              let* strs = extract_strings items in
              Ok (VString (String.concat sep strs), env)
          | v -> Error ("string_concat: 需要 (分隔符, 字符串列表) 元组，但得到 " ^ type_of_value v) ) )
  ; ( "string_split",
      VBuiltin
        ( "string_split",
          fun env -> function
          | VTuple [VString sep; VString s] ->
              let parts = Core.String.split s ~on:(sep.[0]) in
              Ok (VList (List.map (fun p -> VString p) parts), env)
          | v -> Error ("string_split: 需要 (分隔符, 字符串) 元组，但得到 " ^ type_of_value v) ) )
  ; ( "string_contains",
      VBuiltin
        ( "string_contains",
          fun env -> function
          | VTuple [VString substr; VString s] -> Ok (VBool (Core.String.is_substring s ~substring:substr), env)
          | v -> Error ("string_contains: 需要 (子串, 字符串) 元组，但得到 " ^ type_of_value v) ) )
  ; ( "string_replace",
      VBuiltin
        ( "string_replace",
          fun env -> function
          | VTuple [VString old_s; VString new_s; VString s] -> Ok (VString (Core.String.substr_replace_all s ~pattern:old_s ~with_:new_s), env)
          | v -> Error ("string_replace: 需要 (旧字符串, 新字符串, 字符串) 元组，但得到 " ^ type_of_value v) ) )
  ; ( "take",
      VBuiltin
        ( "take",
          fun env -> function
          | VTuple [VInt n; VList items] when n >= 0 -> Ok (VList (Core.List.take items n), env)
          | VTuple [VInt n; VList _] when n < 0 -> Error "take: 参数不能为负数"
          | v -> Error ("take: 需要 (整数, 列表) 元组，但得到 " ^ type_of_value v) ) )
  ; ( "drop",
      VBuiltin
        ( "drop",
          fun env -> function
          | VTuple [VInt n; VList items] when n >= 0 -> Ok (VList (Core.List.drop items n), env)
          | VTuple [VInt n; VList _] when n < 0 -> Error "drop: 参数不能为负数"
          | v -> Error ("drop: 需要 (整数, 列表) 元组，但得到 " ^ type_of_value v) ) )
  ; ( "find",
      VBuiltin
        ( "find",
          fun env -> function
          | VTuple [f; VList items] ->
              let rec find_loop = function
                | [] -> Ok (VCtor ("None", None), env)
                | h :: t ->
                    let* (v, _) = ctx.apply_fn env f h in
                    (match v with
                     | VBool true -> Ok (VCtor ("Some", Some h), env)
                     | VBool false -> find_loop t
                     | _ -> Error "find: 谓词函数必须返回布尔值")
              in
              find_loop items
          | v -> Error ("find: 需要 (函数, 列表) 元组，但得到 " ^ type_of_value v) ) )
  ; ( "exists",
      VBuiltin
        ( "exists",
          fun env -> function
          | VTuple [f; VList items] ->
              let rec exists_loop = function
                | [] -> Ok (VBool false, env)
                | h :: t ->
                    let* (v, _) = ctx.apply_fn env f h in
                    (match v with
                     | VBool true -> Ok (VBool true, env)
                     | VBool false -> exists_loop t
                     | _ -> Error "exists: 谓词函数必须返回布尔值")
              in
              exists_loop items
          | v -> Error ("exists: 需要 (函数, 列表) 元组，但得到 " ^ type_of_value v) ) )
  ; ( "forall",
      VBuiltin
        ( "forall",
          fun env -> function
          | VTuple [f; VList items] ->
              let rec forall_loop = function
                | [] -> Ok (VBool true, env)
                | h :: t ->
                    let* (v, _) = ctx.apply_fn env f h in
                    (match v with
                     | VBool true -> forall_loop t
                     | VBool false -> Ok (VBool false, env)
                     | _ -> Error "forall: 谓词函数必须返回布尔值")
              in
              forall_loop items
          | v -> Error ("forall: 需要 (函数, 列表) 元组，但得到 " ^ type_of_value v) ) )
  ; ( "sort",
      VBuiltin
        ( "sort",
          fun env -> function
          | VList items ->
              let sorted = Core.List.sort ~compare:(fun a b -> match a, b with VInt x, VInt y -> Int.compare x y | VString x, VString y -> String.compare x y | _ -> 0) items in
              Ok (VList sorted, env)
          | v -> Error ("sort: 需要列表，但得到 " ^ type_of_value v) ) )
  ; ( "zip",
      VBuiltin
        ( "zip",
          fun env -> function
          | VTuple [VList a; VList b] ->
              let zipped = Core.List.map2_exn ~f:(fun x y -> VTuple [x; y]) a b in
              Ok (VList zipped, env)
          | v -> Error ("zip: 需要 (列表, 列表) 元组，但得到 " ^ type_of_value v) ) )
  ; ( "abs",
      VBuiltin
        ( "abs",
          fun env -> function
          | VInt n -> Ok (VInt (Int.abs n), env)
          | v -> Error ("abs: 需要整数，但得到 " ^ type_of_value v) ) )
  ; ( "min",
      VBuiltin
        ( "min",
          fun env -> function
          | VTuple [VInt x; VInt y] -> Ok (VInt (Int.min x y), env)
          | VTuple [VString x; VString y] -> Ok (VString (if String.compare x y <= 0 then x else y), env)
          | v -> Error ("min: 需要两个整数或两个字符串的元组，但得到 " ^ type_of_value v) ) )
  ; ( "max",
      VBuiltin
        ( "max",
          fun env -> function
          | VTuple [VInt x; VInt y] -> Ok (VInt (Int.max x y), env)
          | VTuple [VString x; VString y] -> Ok (VString (if String.compare x y >= 0 then x else y), env)
          | v -> Error ("max: 需要两个整数或两个字符串的元组，但得到 " ^ type_of_value v) ) )
  ; ( "int_of_string",
      VBuiltin
        ( "int_of_string",
          fun env -> function
          | VString s -> (try Ok (VInt (int_of_string s), env) with Failure _ -> Error ("int_of_string: 无效的整数字符串: " ^ s))
          | v -> Error ("int_of_string: 需要字符串，但得到 " ^ type_of_value v) ) )
  ; ( "string_of_int",
      VBuiltin
        ( "string_of_int",
          fun env -> function
          | VInt n -> Ok (VString (Int.to_string n), env)
          | v -> Error ("string_of_int: 需要整数，但得到 " ^ type_of_value v) ) )
  ; ( "int_of_char",
      VBuiltin
        ( "int_of_char",
          fun env -> function
          | VChar c -> Ok (VInt (Char.code c), env)
          | v -> Error ("int_of_char: 需要字符，但得到 " ^ type_of_value v) ) )
   ; ( "char_of_int",
      VBuiltin
        ( "char_of_int",
          fun env -> function
          | VInt n -> if n >= 0 && n <= 255 then Ok (VChar (Char.chr n), env) else Error "char_of_int: 超出字符范围 (0-255)"
          | v -> Error ("char_of_int: 需要整数，但得到 " ^ type_of_value v) ) )
   ; ( "sqrt",
      VBuiltin
        ( "sqrt",
          fun env -> function
          | VInt n -> if n >= 0 then Ok (VInt (int_of_float (sqrt (float_of_int n))), env) else Error "sqrt: 不能对负数开方"
          | v -> Error ("sqrt: 需要整数，但得到 " ^ type_of_value v) ) )
   ; ( "pow",
      VBuiltin
        ( "pow",
          fun env -> function
          | VTuple [VInt base; VInt exp] -> Ok (VInt (int_of_float ((float_of_int base) ** (float_of_int exp))), env)
          | v -> Error ("pow: 需要两个整数，但得到 " ^ type_of_value v) ) )
   ; ( "random_int",
      VBuiltin
        ( "random_int",
          fun env -> function
          | VTuple [VInt min; VInt max] ->
              if min <= max then Ok (VInt (min + Random.int (max - min + 1)), env)
              else Error "random_int: 最小值不能大于最大值"
          | v -> Error ("random_int: 需要两个整数，但得到 " ^ type_of_value v) ) )
   ; ( "current_time",
      VBuiltin
        ( "current_time",
          fun env -> function
          | VTuple [] -> Ok (VInt (int_of_float (Unix.gettimeofday ())), env)
          | VUnit -> Ok (VInt (int_of_float (Unix.gettimeofday ())), env)
          | v -> Error ("current_time: 需要 unit，但得到 " ^ type_of_value v) ) )
   ; ( "sleep",
      VBuiltin
        ( "sleep",
          fun env -> function
          | VInt ms -> (Unix.sleepf (float_of_int ms /. 1000.0); Ok (VUnit, env))
          | v -> Error ("sleep: 需要整数（毫秒），但得到 " ^ type_of_value v) ) )
   ; ( "file_exists",
      VBuiltin
        ( "file_exists",
          fun env -> function
          | VString path -> Ok (VBool (Stdlib.Sys.file_exists path), env)
          | v -> Error ("file_exists: 需要字符串，但得到 " ^ type_of_value v) ) )
   ; ( "file_size",
      VBuiltin
        ( "file_size",
          fun env -> function
          | VString path ->
              (try
                 let ic = Stdlib.open_in path in
                 let size = Stdlib.in_channel_length ic in
                 Stdlib.close_in ic;
                 Ok (VInt size, env)
               with _ -> Error ("file_size: 无法获取文件大小: " ^ path))
          | v -> Error ("file_size: 需要字符串，但得到 " ^ type_of_value v) ) )
   ; ( "delete_file",
      VBuiltin
        ( "delete_file",
          fun env -> function
          | VString path -> (Stdlib.Sys.remove path; Ok (VUnit, env))
          | v -> Error ("delete_file: 需要字符串，但得到 " ^ type_of_value v) ) )
   ; ( "list_directory",
      VBuiltin
        ( "list_directory",
          fun env -> function
          | VString path ->
              (try
                 let files = Stdlib.Sys.readdir path |> Array.to_list in
                 Ok (VList (List.map (fun f -> VString f) files), env)
               with _ -> Error ("list_directory: 无法读取目录: " ^ path))
          | v -> Error ("list_directory: 需要字符串，但得到 " ^ type_of_value v) ) )
   ; ( "get_env",
      VBuiltin
        ( "get_env",
          fun env -> function
          | VString var ->
              (try
                 let value = Stdlib.Sys.getenv var in
                 Ok (VCtor ("Some", Some (VString value)), env)
               with Not_found -> Ok (VCtor ("None", None), env))
          | v -> Error ("get_env: 需要字符串，但得到 " ^ type_of_value v) ) )
   ; ( "system_command",
      VBuiltin
        ( "system_command",
          fun env -> function
          | VString cmd -> 
              let status = Unix.system cmd in
              let code = match status with
                | Unix.WEXITED n -> n
                | Unix.WSIGNALED _ | Unix.WSTOPPED _ -> -1
              in
              Ok (VInt code, env)
          | v -> Error ("system_command: 需要字符串，但得到 " ^ type_of_value v) ) )
     ; ( "regex_match",
       VBuiltin
         ( "regex_match",
           fun env -> function
           | VTuple [VString pattern; VString text] ->
               (try
                  let re = Str.regexp pattern in
                  Ok (VBool (Str.string_match re text 0), env)
                with _ -> Error ("regex_match: 无效的正则表达式: " ^ pattern))
           | v -> Error ("regex_match: 需要(模式, 文本)，但得到 " ^ type_of_value v) ) )
    ; ( "regex_replace",
       VBuiltin
         ( "regex_replace",
           fun env -> function
           | VTuple [VString pattern; VString replacement; VString text] ->
               (try
                  let re = Str.regexp pattern in
                  Ok (VString (Str.global_replace re replacement text), env)
                with _ -> Error ("regex_replace: 无效的正则表达式: " ^ pattern))
           | v -> Error ("regex_replace: 需要(模式, 替换, 文本)，但得到 " ^ type_of_value v) ) )
    ; ( "regex_split",
       VBuiltin
         ( "regex_split",
           fun env -> function
           | VTuple [VString pattern; VString text] ->
               (try
                  let re = Str.regexp pattern in
                  let parts = Str.split re text in
                  Ok (VList (List.map (fun s -> VString s) parts), env)
                with _ -> Error ("regex_split: 无效的正则表达式: " ^ pattern))
           | v -> Error ("regex_split: 需要(模式, 文本)，但得到 " ^ type_of_value v) ) )
    ]
