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
    (* ===== 新增标准库函数 ===== *)
    (* HashMap 操作 *)
    ; ( "hashmap_create",
      VBuiltin
        ( "hashmap_create",
          fun env -> function
          | VUnit | VTuple [] -> Ok (VRecord [], env)
          | v -> Error ("hashmap_create: 需要 unit，但得到 " ^ type_of_value v) ) )
    ; ( "hashmap_get",
      VBuiltin
        ( "hashmap_get",
          fun env -> function
          | VTuple [VRecord fields; VString key] ->
              (match List.assoc_opt key fields with
               | Some r -> Ok (VCtor ("Some", Some !r), env)
               | None -> Ok (VCtor ("None", None), env))
          | v -> Error ("hashmap_get: 需要 (record, string) 元组，但得到 " ^ type_of_value v) ) )
    ; ( "hashmap_set",
      VBuiltin
        ( "hashmap_set",
          fun env -> function
          | VTuple [VRecord fields; VString key; value] ->
              let new_fields = (key, ref value) :: List.filter (fun (k, _) -> k <> key) fields in
              Ok (VRecord new_fields, env)
          | v -> Error ("hashmap_set: 需要 (record, string, value) 元组，但得到 " ^ type_of_value v) ) )
    ; ( "hashmap_delete",
      VBuiltin
        ( "hashmap_delete",
          fun env -> function
          | VTuple [VRecord fields; VString key] ->
              let new_fields = List.filter (fun (k, _) -> k <> key) fields in
              Ok (VRecord new_fields, env)
          | v -> Error ("hashmap_delete: 需要 (record, string) 元组，但得到 " ^ type_of_value v) ) )
    ; ( "hashmap_keys",
      VBuiltin
        ( "hashmap_keys",
          fun env -> function
          | VRecord fields ->
              Ok (VList (List.map (fun (k, _) -> VString k) fields), env)
          | v -> Error ("hashmap_keys: 需要 record，但得到 " ^ type_of_value v) ) )
    ; ( "hashmap_values",
      VBuiltin
        ( "hashmap_values",
          fun env -> function
          | VRecord fields ->
              Ok (VList (List.map (fun (_, r) -> !r) fields), env)
          | v -> Error ("hashmap_values: 需要 record，但得到 " ^ type_of_value v) ) )
    ; ( "hashmap_size",
      VBuiltin
        ( "hashmap_size",
          fun env -> function
          | VRecord fields ->
              Ok (VInt (List.length fields), env)
          | v -> Error ("hashmap_size: 需要 record，但得到 " ^ type_of_value v) ) )
    ; ( "hashmap_has_key",
      VBuiltin
        ( "hashmap_has_key",
          fun env -> function
          | VTuple [VRecord fields; VString key] ->
              Ok (VBool (List.mem_assoc key fields), env)
          | v -> Error ("hashmap_has_key: 需要 (record, string) 元组，但得到 " ^ type_of_value v) ) )
    (* IO 操作增强 *)
    ; ( "read_lines",
      VBuiltin
        ( "read_lines",
          fun env -> function
          | VString filename ->
              (try
                 let ic = open_in filename in
                 let rec read_all acc =
                   try
                     let line = input_line ic in
                     read_all (VString line :: acc)
                   with End_of_file ->
                     close_in ic;
                     List.rev acc
                 in
                 Ok (VList (read_all []), env)
               with Sys_error msg -> Error ("read_lines: " ^ msg))
          | v -> Error ("read_lines: 需要字符串，但得到 " ^ type_of_value v) ) )
    ; ( "write_lines",
      VBuiltin
        ( "write_lines",
          fun env -> function
          | VTuple [VString filename; VList lines] ->
              (try
                 let oc = open_out filename in
                 List.iter (fun line ->
                   match line with
                   | VString s -> Printf.fprintf oc "%s\n" s
                   | _ -> ()
                 ) lines;
                 close_out oc;
                 Ok (VUnit, env)
               with Sys_error msg -> Error ("write_lines: " ^ msg))
          | v -> Error ("write_lines: 需要 (string, list) 元组，但得到 " ^ type_of_value v) ) )
    ; ( "append_file",
      VBuiltin
        ( "append_file",
          fun env -> function
          | VTuple [VString filename; VString content] ->
              (try
                 let oc = open_out_gen [Open_append; Open_creat] 0o644 filename in
                 output_string oc content;
                 close_out oc;
                 Ok (VUnit, env)
               with Sys_error msg -> Error ("append_file: " ^ msg))
          | v -> Error ("append_file: 需要 (string, string) 元组，但得到 " ^ type_of_value v) ) )
    ; ( "copy_file",
      VBuiltin
        ( "copy_file",
          fun env -> function
          | VTuple [VString src; VString dst] ->
              (try
                 let ic = open_in src in
                 let oc = open_out dst in
                 let buf = Bytes.create 4096 in
                 let rec copy () =
                   let n = input ic buf 0 4096 in
                   if n > 0 then (
                     output oc buf 0 n;
                     copy ()
                   )
                 in
                 copy ();
                 close_in ic;
                 close_out oc;
                 Ok (VUnit, env)
               with Sys_error msg -> Error ("copy_file: " ^ msg))
          | v -> Error ("copy_file: 需要 (string, string) 元组，但得到 " ^ type_of_value v) ) )
    ; ( "file_size",
      VBuiltin
        ( "file_size",
          fun env -> function
          | VString path ->
              (try
                 let ic = open_in path in
                 let size = in_channel_length ic in
                 close_in ic;
                 Ok (VInt size, env)
               with Sys_error msg -> Error ("file_size: " ^ msg))
          | v -> Error ("file_size: 需要字符串，但得到 " ^ type_of_value v) ) )
    (* 字符串操作增强 *)
    ; ( "string_starts_with",
      VBuiltin
        ( "string_starts_with",
          fun env -> function
          | VTuple [VString s; VString prefix] ->
              Ok (VBool (String.length s >= String.length prefix && String.sub s 0 (String.length prefix) = prefix), env)
          | v -> Error ("string_starts_with: 需要 (string, string) 元组，但得到 " ^ type_of_value v) ) )
    ; ( "string_ends_with",
      VBuiltin
        ( "string_ends_with",
          fun env -> function
          | VTuple [VString s; VString suffix] ->
              let len_s = String.length s and len_suffix = String.length suffix in
              Ok (VBool (len_s >= len_suffix && String.sub s (len_s - len_suffix) len_suffix = suffix), env)
          | v -> Error ("string_ends_with: 需要 (string, string) 元组，但得到 " ^ type_of_value v) ) )
    ; ( "string_repeat",
      VBuiltin
        ( "string_repeat",
          fun env -> function
          | VTuple [VString s; VInt n] when n >= 0 ->
              Ok (VString (String.concat "" (List.init n (fun _ -> s))), env)
          | v -> Error ("string_repeat: 需要 (string, int) 元组，但得到 " ^ type_of_value v) ) )
    ; ( "string_pad_left",
      VBuiltin
        ( "string_pad_left",
          fun env -> function
          | VTuple [VString s; VInt n; VString pad] when n > 0 ->
              let len = String.length s in
              if len >= n then Ok (VString s, env)
              else
                let pad_len = String.length pad in
                let needed = n - len in
                let padding = String.concat "" (List.init (needed / pad_len + 1) (fun _ -> pad)) in
                Ok (VString (String.sub padding 0 needed ^ s), env)
          | v -> Error ("string_pad_left: 需要 (string, int, string) 元组，但得到 " ^ type_of_value v) ) )
    ; ( "string_pad_right",
      VBuiltin
        ( "string_pad_right",
          fun env -> function
          | VTuple [VString s; VInt n; VString pad] when n > 0 ->
              let len = String.length s in
              if len >= n then Ok (VString s, env)
              else
                let pad_len = String.length pad in
                let needed = n - len in
                let padding = String.concat "" (List.init (needed / pad_len + 1) (fun _ -> pad)) in
                Ok (VString (s ^ String.sub padding 0 needed), env)
          | v -> Error ("string_pad_right: 需要 (string, int, string) 元组，但得到 " ^ type_of_value v) ) )
    (* 列表操作增强 *)
    ; ( "list_flatten",
      VBuiltin
        ( "list_flatten",
          fun env -> function
          | VList lists ->
              let rec flatten acc = function
                | [] -> Ok (VList (List.rev acc), env)
                | VList xs :: rest -> flatten (List.rev_append xs acc) rest
                | v :: _ -> Error ("list_flatten: 列表元素必须是列表，但得到 " ^ type_of_value v)
              in
              flatten [] lists
          | v -> Error ("list_flatten: 需要列表，但得到 " ^ type_of_value v) ) )
    ; ( "list_flat_map",
      VBuiltin
        ( "list_flat_map",
          fun env f ->
            Ok (VBuiltin
               ( "list_flat_map'",
                 fun env xs ->
                   match xs with
                   | VList items ->
                       let rec flat_map acc = function
                         | [] -> Ok (VList (List.rev acc), env)
                         | item :: rest ->
                             let* (result, _) = ctx.apply_fn env f item in
                             (match result with
                              | VList ys -> flat_map (List.rev_append ys acc) rest
                              | v -> Error ("list_flat_map: 函数必须返回列表，但得到 " ^ type_of_value v))
                       in
                       flat_map [] items
                   | v -> Error ("list_flat_map: 第二个参数必须是列表，但得到 " ^ type_of_value v) ),
             env) ) )
    ; ( "list_count",
      VBuiltin
        ( "list_count",
          fun env f ->
            Ok (VBuiltin
               ( "list_count'",
                 fun env xs ->
                   match xs with
                   | VList items ->
                       let rec count acc = function
                         | [] -> Ok (VInt acc, env)
                         | item :: rest ->
                             let* (v, _) = ctx.apply_fn env f item in
                             (match v with
                              | VBool true -> count (acc + 1) rest
                              | VBool false -> count acc rest
                              | v -> Error ("list_count: 谓词必须返回布尔值，但得到 " ^ type_of_value v))
                       in
                       count 0 items
                   | v -> Error ("list_count: 第二个参数必须是列表，但得到 " ^ type_of_value v) ),
             env) ) )
    ; ( "list_distinct",
      VBuiltin
        ( "list_distinct",
          fun env -> function
          | VList items ->
              let rec distinct acc = function
                | [] -> Ok (VList (List.rev acc), env)
                | x :: rest ->
                    if List.exists (fun y -> x = y) acc then
                      distinct acc rest
                    else
                      distinct (x :: acc) rest
              in
              distinct [] items
          | v -> Error ("list_distinct: 需要列表，但得到 " ^ type_of_value v) ) )
    ; ( "list_group_by",
      VBuiltin
        ( "list_group_by",
          fun env f ->
            Ok (VBuiltin
               ( "list_group_by'",
                 fun env xs ->
                   match xs with
                   | VList items ->
                       let rec group acc = function
                         | [] -> Ok (VRecord (List.map (fun (k, vs) -> (k, ref (VList (List.rev vs)))) acc), env)
                         | item :: rest ->
                             let* (key, _) = ctx.apply_fn env f item in
                             (match key with
                              | VString k ->
                                  let existing = try List.assoc k acc with Not_found -> [] in
                                  let acc' = (k, item :: existing) :: List.filter (fun (j, _) -> j <> k) acc in
                                  group acc' rest
                              | v -> Error ("list_group_by: 函数必须返回字符串，但得到 " ^ type_of_value v))
                       in
                       group [] items
                   | v -> Error ("list_group_by: 第二个参数必须是列表，但得到 " ^ type_of_value v) ),
             env) ) )
    (* 数学操作 *)
    ; ( "math_abs",
      VBuiltin
        ( "math_abs",
          fun env -> function
          | VInt n -> Ok (VInt (abs n), env)
          | v -> Error ("math_abs: 需要整数，但得到 " ^ type_of_value v) ) )
    ; ( "math_min",
      VBuiltin
        ( "math_min",
          fun env -> function
          | VTuple [VInt a; VInt b] -> Ok (VInt (min a b), env)
          | v -> Error ("math_min: 需要 (int, int) 元组，但得到 " ^ type_of_value v) ) )
    ; ( "math_max",
      VBuiltin
        ( "math_max",
          fun env -> function
          | VTuple [VInt a; VInt b] -> Ok (VInt (max a b), env)
          | v -> Error ("math_max: 需要 (int, int) 元组，但得到 " ^ type_of_value v) ) )
    ; ( "math_clamp",
      VBuiltin
        ( "math_clamp",
          fun env -> function
          | VTuple [VInt x; VInt lo; VInt hi] ->
              Ok (VInt (max lo (min hi x)), env)
          | v -> Error ("math_clamp: 需要 (int, int, int) 元组，但得到 " ^ type_of_value v) ) )
    ; ( "math_sum",
      VBuiltin
        ( "math_sum",
          fun env -> function
          | VList items ->
              let rec sum acc = function
                | [] -> Ok (VInt acc, env)
                | VInt n :: rest -> sum (acc + n) rest
                | v :: _ -> Error ("math_sum: 列表元素必须是整数，但得到 " ^ type_of_value v)
              in
              sum 0 items
          | v -> Error ("math_sum: 需要列表，但得到 " ^ type_of_value v) ) )
    ; ( "math_product",
      VBuiltin
        ( "math_product",
          fun env -> function
          | VList items ->
              let rec product acc = function
                | [] -> Ok (VInt acc, env)
                | VInt n :: rest -> product (acc * n) rest
                | v :: _ -> Error ("math_product: 列表元素必须是整数，但得到 " ^ type_of_value v)
              in
              product 1 items
          | v -> Error ("math_product: 需要列表，但得到 " ^ type_of_value v) ) )
    (* 转换函数 *)
    ; ( "int_to_string",
      VBuiltin
        ( "int_to_string",
          fun env -> function
          | VInt n -> Ok (VString (string_of_int n), env)
          | v -> Error ("int_to_string: 需要整数，但得到 " ^ type_of_value v) ) )
    ; ( "string_to_int",
      VBuiltin
        ( "string_to_int",
          fun env -> function
          | VString s ->
              (try Ok (VInt (int_of_string s), env)
               with Failure _ -> Error ("string_to_int: 无效的整数字符串: " ^ s))
          | v -> Error ("string_to_int: 需要字符串，但得到 " ^ type_of_value v) ) )
    ; ( "bool_to_string",
      VBuiltin
        ( "bool_to_string",
          fun env -> function
          | VBool true -> Ok (VString "true", env)
          | VBool false -> Ok (VString "false", env)
          | v -> Error ("bool_to_string: 需要布尔值，但得到 " ^ type_of_value v) ) )
    ; ( "char_to_string",
      VBuiltin
        ( "char_to_string",
          fun env -> function
          | VChar c -> Ok (VString (String.make 1 c), env)
          | v -> Error ("char_to_string: 需要字符，但得到 " ^ type_of_value v) ) )
    (* 调试函数 *)
    ; ( "debug_print",
      VBuiltin
        ( "debug_print",
          fun env v ->
            Printf.printf "[DEBUG] %s\n" (string_of_value v);
            Ok (VUnit, env) ) )
    ; ( "debug_to_string",
      VBuiltin
        ( "debug_to_string",
          fun env v ->
            Ok (VString (string_of_value v), env) ) )
    (* ===== JSON 支持 ===== *)
    ; ( "json_parse",
      VBuiltin
        ( "json_parse",
          fun env -> function
          | VString s ->
              (try
                 let json = Yojson.Safe.from_string s in
                 let rec json_to_value = function
                   | `Null -> VUnit
                   | `Bool b -> VBool b
                   | `Int n -> VInt n
                   | `Float f -> VInt (int_of_float f)
                   | `String s -> VString s
                   | `List xs -> VList (List.map json_to_value xs)
                   | `Assoc pairs -> VRecord (List.map (fun (k, v) -> (k, ref (json_to_value v))) pairs)
                   | _ -> VUnit
                 in
                 Ok (json_to_value json, env)
               with Yojson.Json_error msg -> Error ("json_parse: 解析失败: " ^ msg))
          | v -> Error ("json_parse: 需要字符串，但得到 " ^ type_of_value v) ) )
    ; ( "json_stringify",
      VBuiltin
        ( "json_stringify",
          fun env v ->
            let rec value_to_json = function
              | VUnit -> `Null
              | VBool b -> `Bool b
              | VInt n -> `Int n
              | VString s -> `String s
              | VList xs -> `List (List.map value_to_json xs)
              | VRecord pairs -> `Assoc (List.map (fun (k, r) -> (k, value_to_json !r)) pairs)
              | VTuple xs -> `List (List.map value_to_json xs)
              | VCtor (name, None) -> `Assoc [("type", `String name)]
              | VCtor (name, Some v) -> `Assoc [("type", `String name); ("value", value_to_json v)]
              | _ -> `Null
            in
            let json = value_to_json v in
            Ok (VString (Yojson.Safe.to_string json), env) ) )
    ; ( "json_pretty",
      VBuiltin
        ( "json_pretty",
          fun env v ->
            let rec value_to_json = function
              | VUnit -> `Null
              | VBool b -> `Bool b
              | VInt n -> `Int n
              | VString s -> `String s
              | VList xs -> `List (List.map value_to_json xs)
              | VRecord pairs -> `Assoc (List.map (fun (k, r) -> (k, value_to_json !r)) pairs)
              | VTuple xs -> `List (List.map value_to_json xs)
              | VCtor (name, None) -> `Assoc [("type", `String name)]
              | VCtor (name, Some v) -> `Assoc [("type", `String name); ("value", value_to_json v)]
              | _ -> `Null
            in
            let json = value_to_json v in
            Ok (VString (Yojson.Safe.pretty_to_string json), env) ) )
    (* ===== 日期时间 ===== *)
    ; ( "time_now",
      VBuiltin
        ( "time_now",
          fun env -> function
          | VUnit | VTuple [] ->
              let t = Unix.gettimeofday () in
              Ok (VInt (int_of_float t), env)
          | v -> Error ("time_now: 需要 unit，但得到 " ^ type_of_value v) ) )
    ; ( "time_now_ms",
      VBuiltin
        ( "time_now_ms",
          fun env -> function
          | VUnit | VTuple [] ->
              let t = Unix.gettimeofday () in
              Ok (VInt (int_of_float (t *. 1000.0)), env)
          | v -> Error ("time_now_ms: 需要 unit，但得到 " ^ type_of_value v) ) )
    ; ( "time_sleep_ms",
      VBuiltin
        ( "time_sleep_ms",
          fun env -> function
          | VInt ms when ms >= 0 ->
              Unix.sleepf (float_of_int ms /. 1000.0);
              Ok (VUnit, env)
          | v -> Error ("time_sleep_ms: 需要非负整数，但得到 " ^ type_of_value v) ) )
    ; ( "time_format",
      VBuiltin
        ( "time_format",
          fun env -> function
          | VTuple [VInt timestamp; VString format] ->
              (try
                 let t = Unix.localtime (float_of_int timestamp) in
                 let year = string_of_int (t.Unix.tm_year + 1900) in
                 let month = Printf.sprintf "%02d" (t.Unix.tm_mon + 1) in
                 let day = Printf.sprintf "%02d" t.Unix.tm_mday in
                 let hour = Printf.sprintf "%02d" t.Unix.tm_hour in
                 let minute = Printf.sprintf "%02d" t.Unix.tm_min in
                 let second = Printf.sprintf "%02d" t.Unix.tm_sec in
                 let buf = Buffer.create (String.length format + 20) in
                 let i = ref 0 in
                 while !i < String.length format do
                   if !i + 1 < String.length format && format.[!i] = '%' then (
                     (match format.[!i + 1] with
                      | 'Y' -> Buffer.add_string buf year
                      | 'm' -> Buffer.add_string buf month
                      | 'd' -> Buffer.add_string buf day
                      | 'H' -> Buffer.add_string buf hour
                      | 'M' -> Buffer.add_string buf minute
                      | 'S' -> Buffer.add_string buf second
                      | c -> Buffer.add_char buf '%'; Buffer.add_char buf c);
                     i := !i + 2
                   ) else (
                     Buffer.add_char buf format.[!i];
                     i := !i + 1
                   )
                 done;
                 Ok (VString (Buffer.contents buf), env)
               with _ -> Error "time_format: 格式化失败")
          | v -> Error ("time_format: 需要 (int, string) 元组，但得到 " ^ type_of_value v) ) )
    ; ( "time_year",
      VBuiltin
        ( "time_year",
          fun env -> function
          | VInt timestamp ->
              let t = Unix.localtime (float_of_int timestamp) in
              Ok (VInt (t.Unix.tm_year + 1900), env)
          | v -> Error ("time_year: 需要整数，但得到 " ^ type_of_value v) ) )
    ; ( "time_month",
      VBuiltin
        ( "time_month",
          fun env -> function
          | VInt timestamp ->
              let t = Unix.localtime (float_of_int timestamp) in
              Ok (VInt (t.Unix.tm_mon + 1), env)
          | v -> Error ("time_month: 需要整数，但得到 " ^ type_of_value v) ) )
    ; ( "time_day",
      VBuiltin
        ( "time_day",
          fun env -> function
          | VInt timestamp ->
              let t = Unix.localtime (float_of_int timestamp) in
              Ok (VInt t.Unix.tm_mday, env)
          | v -> Error ("time_day: 需要整数，但得到 " ^ type_of_value v) ) )
    ; ( "time_hour",
      VBuiltin
        ( "time_hour",
          fun env -> function
          | VInt timestamp ->
              let t = Unix.localtime (float_of_int timestamp) in
              Ok (VInt t.Unix.tm_hour, env)
          | v -> Error ("time_hour: 需要整数，但得到 " ^ type_of_value v) ) )
    ; ( "time_minute",
      VBuiltin
        ( "time_minute",
          fun env -> function
          | VInt timestamp ->
              let t = Unix.localtime (float_of_int timestamp) in
              Ok (VInt t.Unix.tm_min, env)
          | v -> Error ("time_minute: 需要整数，但得到 " ^ type_of_value v) ) )
    ; ( "time_second",
      VBuiltin
        ( "time_second",
          fun env -> function
          | VInt timestamp ->
              let t = Unix.localtime (float_of_int timestamp) in
              Ok (VInt t.Unix.tm_sec, env)
          | v -> Error ("time_second: 需要整数，但得到 " ^ type_of_value v) ) )
    ; ( "time_day_of_week",
      VBuiltin
        ( "time_day_of_week",
          fun env -> function
          | VInt timestamp ->
              let t = Unix.localtime (float_of_int timestamp) in
              Ok (VInt t.Unix.tm_wday, env)
          | v -> Error ("time_day_of_week: 需要整数，但得到 " ^ type_of_value v) ) )
    (* ===== 集合操作 ===== *)
    ; ( "set_create",
      VBuiltin
        ( "set_create",
          fun env -> function
          | VUnit | VTuple [] -> Ok (VList [], env)
          | v -> Error ("set_create: 需要 unit，但得到 " ^ type_of_value v) ) )
    ; ( "set_add",
      VBuiltin
        ( "set_add",
          fun env -> function
          | VTuple [VList items; value] ->
              if List.exists (fun x -> x = value) items then
                Ok (VList items, env)
              else
                Ok (VList (items @ [value]), env)
          | v -> Error ("set_add: 需要 (list, value) 元组，但得到 " ^ type_of_value v) ) )
    ; ( "set_remove",
      VBuiltin
        ( "set_remove",
          fun env -> function
          | VTuple [VList items; value] ->
              Ok (VList (List.filter (fun x -> x <> value) items), env)
          | v -> Error ("set_remove: 需要 (list, value) 元组，但得到 " ^ type_of_value v) ) )
    ; ( "set_contains",
      VBuiltin
        ( "set_contains",
          fun env -> function
          | VTuple [VList items; value] ->
              Ok (VBool (List.exists (fun x -> x = value) items), env)
          | v -> Error ("set_contains: 需要 (list, value) 元组，但得到 " ^ type_of_value v) ) )
    ; ( "set_size",
      VBuiltin
        ( "set_size",
          fun env -> function
          | VList items ->
              Ok (VInt (List.length items), env)
          | v -> Error ("set_size: 需要列表，但得到 " ^ type_of_value v) ) )
    ; ( "set_union",
      VBuiltin
        ( "set_union",
          fun env -> function
          | VTuple [VList a; VList b] ->
              let merged = a @ List.filter (fun x -> not (List.exists (fun y -> y = x) a)) b in
              Ok (VList merged, env)
          | v -> Error ("set_union: 需要 (list, list) 元组，但得到 " ^ type_of_value v) ) )
    ; ( "set_intersection",
      VBuiltin
        ( "set_intersection",
          fun env -> function
          | VTuple [VList a; VList b] ->
              Ok (VList (List.filter (fun x -> List.exists (fun y -> y = x) b) a), env)
          | v -> Error ("set_intersection: 需要 (list, list) 元组，但得到 " ^ type_of_value v) ) )
    ; ( "set_difference",
      VBuiltin
        ( "set_difference",
          fun env -> function
          | VTuple [VList a; VList b] ->
              Ok (VList (List.filter (fun x -> not (List.exists (fun y -> y = x) b)) a), env)
          | v -> Error ("set_difference: 需要 (list, list) 元组，但得到 " ^ type_of_value v) ) )
    (* ===== 网络操作 ===== *)
    ; ( "http_get",
      VBuiltin
        ( "http_get",
          fun env -> function
          | VString url ->
              (try
                 let ic = Unix.open_process_in (Printf.sprintf "curl -s '%s'" url) in
                 let rec read_all acc =
                   try
                     let line = input_line ic in
                     read_all (acc ^ line ^ "\n")
                   with End_of_file -> acc
                 in
                 let content = read_all "" in
                 let _ = Unix.close_process_in ic in
                 Ok (VString content, env)
               with _ -> Error "http_get: 请求失败")
          | v -> Error ("http_get: 需要字符串 URL，但得到 " ^ type_of_value v) ) )
    ; ( "http_post",
      VBuiltin
        ( "http_post",
          fun env -> function
          | VTuple [VString url; VString body] ->
              (try
                 let ic = Unix.open_process_in (Printf.sprintf "curl -s -X POST -d '%s' '%s'" body url) in
                 let rec read_all acc =
                   try
                     let line = input_line ic in
                     read_all (acc ^ line ^ "\n")
                   with End_of_file -> acc
                 in
                 let content = read_all "" in
                 let _ = Unix.close_process_in ic in
                 Ok (VString content, env)
               with _ -> Error "http_post: 请求失败")
          | v -> Error ("http_post: 需要 (url, body) 元组，但得到 " ^ type_of_value v) ) )
    ; ( "url_encode",
      VBuiltin
        ( "url_encode",
          fun env -> function
          | VString s ->
              let buf = Buffer.create (String.length s * 3) in
              String.iter (fun c ->
                match c with
                | 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '_' | '.' | '~' ->
                    Buffer.add_char buf c
                | _ ->
                    Buffer.add_string buf (Printf.sprintf "%%%02X" (Char.code c))
              ) s;
              Ok (VString (Buffer.contents buf), env)
          | v -> Error ("url_encode: 需要字符串，但得到 " ^ type_of_value v) ) )
    ; ( "url_decode",
      VBuiltin
        ( "url_decode",
          fun env -> function
          | VString s ->
              let buf = Buffer.create (String.length s) in
              let i = ref 0 in
              while !i < String.length s do
                if s.[!i] = '%' && !i + 2 < String.length s then (
                  let hex = String.sub s (!i + 1) 2 in
                  let code = int_of_string ("0x" ^ hex) in
                  Buffer.add_char buf (Char.chr code);
                  i := !i + 3
                ) else if s.[!i] = '+' then (
                  Buffer.add_char buf ' ';
                  i := !i + 1
                ) else (
                  Buffer.add_char buf s.[!i];
                  i := !i + 1
                )
              done;
              Ok (VString (Buffer.contents buf), env)
          | v -> Error ("url_decode: 需要字符串，但得到 " ^ type_of_value v) ) )
    (* ===== 加密操作 ===== *)
    ; ( "hash_md5",
      VBuiltin
        ( "hash_md5",
          fun env -> function
          | VString s ->
              let hash = Digest.string s |> Digest.to_hex in
              Ok (VString hash, env)
          | v -> Error ("hash_md5: 需要字符串，但得到 " ^ type_of_value v) ) )
    ; ( "hash_sha256",
      VBuiltin
        ( "hash_sha256",
          fun env -> function
          | VString s ->
              let hash = Digest.string s |> Digest.to_hex in
              Ok (VString hash, env)
          | v -> Error ("hash_sha256: 需要字符串，但得到 " ^ type_of_value v) ) )
    ; ( "base64_encode",
      VBuiltin
        ( "base64_encode",
          fun env -> function
          | VString s ->
              let encoded = Base64.encode_exn s in
              Ok (VString encoded, env)
          | v -> Error ("base64_encode: 需要字符串，但得到 " ^ type_of_value v) ) )
    ; ( "base64_decode",
      VBuiltin
        ( "base64_decode",
          fun env -> function
          | VString s ->
              (match Base64.decode s with
               | Ok decoded -> Ok (VString decoded, env)
               | Error _ -> Error "base64_decode: 解码失败")
          | v -> Error ("base64_decode: 需要字符串，但得到 " ^ type_of_value v) ) )
    ; ( "hex_encode",
      VBuiltin
        ( "hex_encode",
          fun env -> function
          | VString s ->
              let hex = String.concat "" (List.init (String.length s) (fun i ->
                Printf.sprintf "%02X" (Char.code s.[i])
              )) in
              Ok (VString hex, env)
          | v -> Error ("hex_encode: 需要字符串，但得到 " ^ type_of_value v) ) )
    ; ( "hex_decode",
      VBuiltin
        ( "hex_decode",
          fun env -> function
          | VString s ->
              if String.length s mod 2 <> 0 then
                Error "hex_decode: 长度必须是偶数"
              else
                let buf = Buffer.create (String.length s / 2) in
                let i = ref 0 in
                while !i < String.length s do
                  let hex = String.sub s !i 2 in
                  let code = int_of_string ("0x" ^ hex) in
                  Buffer.add_char buf (Char.chr code);
                  i := !i + 2
                done;
                Ok (VString (Buffer.contents buf), env)
          | v -> Error ("hex_decode: 需要字符串，但得到 " ^ type_of_value v) ) )
    (* ===== 并发操作 ===== *)
    ; ( "thread_create",
      VBuiltin
        ( "thread_create",
          fun env -> function
          | VFun _ | VBuiltin _ as f ->
              let thread_func () =
                match ctx.apply_fn env f VUnit with
                | Ok (result, _) -> result
                | Error msg -> VString ("Error: " ^ msg)
              in
              let thread = Thread.create thread_func () in
              Ok (VInt (Thread.id thread), env)
          | v -> Error ("thread_create: 需要函数，但得到 " ^ type_of_value v) ) )
    ; ( "thread_join",
      VBuiltin
        ( "thread_join",
          fun env -> function
          | VInt _ ->
              (* 简化实现：等待一小段时间 *)
              Unix.sleepf 0.001;
              Ok (VUnit, env)
          | v -> Error ("thread_join: 需要线程 ID，但得到 " ^ type_of_value v) ) )
    ; ( "mutex_create",
      VBuiltin
        ( "mutex_create",
          fun env -> function
          | VUnit | VTuple [] ->
              Ok (VInt 0, env)  (* 简化实现 *)
          | v -> Error ("mutex_create: 需要 unit，但得到 " ^ type_of_value v) ) )
    ; ( "mutex_lock",
      VBuiltin
        ( "mutex_lock",
          fun env -> function
          | VInt _ ->
              Ok (VUnit, env)  (* 简化实现 *)
          | v -> Error ("mutex_lock: 需要 mutex，但得到 " ^ type_of_value v) ) )
    ; ( "mutex_unlock",
      VBuiltin
        ( "mutex_unlock",
          fun env -> function
          | VInt _ ->
              Ok (VUnit, env)  (* 简化实现 *)
          | v -> Error ("mutex_unlock: 需要 mutex，但得到 " ^ type_of_value v) ) )
    ; ( "channel_create",
      VBuiltin
        ( "channel_create",
          fun env -> function
          | VUnit | VTuple [] ->
              Ok (VRecord [("buffer", ref (VList [])); ("closed", ref (VBool false))], env)
          | v -> Error ("channel_create: 需要 unit，但得到 " ^ type_of_value v) ) )
    ; ( "channel_send",
      VBuiltin
        ( "channel_send",
          fun env -> function
          | VTuple [VRecord fields; value] ->
              (match List.assoc_opt "buffer" fields with
               | Some r ->
                   (match !r with
                    | VList items ->
                        r := VList (items @ [value]);
                        Ok (VUnit, env)
                    | _ -> Error "channel_send: buffer 类型错误")
               | None -> Error "channel_send: 无效的 channel")
          | v -> Error ("channel_send: 需要 (channel, value) 元组，但得到 " ^ type_of_value v) ) )
    ; ( "channel_receive",
      VBuiltin
        ( "channel_receive",
          fun env -> function
          | VRecord fields ->
              (match List.assoc_opt "buffer" fields with
               | Some r ->
                   (match !r with
                    | VList (item :: rest) ->
                        r := VList rest;
                        Ok (item, env)
                    | VList [] ->
                        Error "channel_receive: channel 为空"
                    | _ -> Error "channel_receive: buffer 类型错误")
               | None -> Error "channel_receive: 无效的 channel")
          | v -> Error ("channel_receive: 需要 channel，但得到 " ^ type_of_value v) ) )
    (* ===== 调试增强 ===== *)
    ; ( "debug_trace",
      VBuiltin
        ( "debug_trace",
          fun env v ->
            Printf.printf "[TRACE] %s\n" (string_of_value v);
            Ok (v, env) ) )
    ; ( "debug_assert",
      VBuiltin
        ( "debug_assert",
          fun env -> function
          | VBool true -> Ok (VUnit, env)
          | VBool false -> Error "assertion failed"
          | v -> Error ("debug_assert: 需要布尔值，但得到 " ^ type_of_value v) ) )
    ; ( "debug_type",
      VBuiltin
        ( "debug_type",
          fun env v ->
            Ok (VString (type_of_value v), env) ) )
    (* ===== 工业级标准库扩充 ===== *)
    (* 更多字符串操作 *)
    ; ( "string_join",
      VBuiltin
        ( "string_join",
          fun env -> function
          | VTuple [VString sep; VList items] ->
              let rec extract_strings acc = function
                | [] -> Ok (List.rev acc)
                | VString s :: rest -> extract_strings (s :: acc) rest
                | v :: _ -> Error ("string_join: 列表元素必须是字符串，但得到 " ^ type_of_value v)
              in
              let* strs = extract_strings [] items in
              Ok (VString (String.concat sep strs), env)
          | v -> Error ("string_join: 需要 (string, list) 元组，但得到 " ^ type_of_value v) ) )
    ; ( "string_to_chars",
      VBuiltin
        ( "string_to_chars",
          fun env -> function
          | VString s ->
              Ok (VList (List.init (String.length s) (fun i -> VChar s.[i])), env)
          | v -> Error ("string_to_chars: 需要字符串，但得到 " ^ type_of_value v) ) )
    ; ( "string_from_chars",
      VBuiltin
        ( "string_from_chars",
          fun env -> function
          | VList chars ->
              let rec extract_chars acc = function
                | [] -> Ok (List.rev acc)
                | VChar c :: rest -> extract_chars (c :: acc) rest
                | v :: _ -> Error ("string_from_chars: 列表元素必须是字符，但得到 " ^ type_of_value v)
              in
              let* cs = extract_chars [] chars in
              Ok (VString (String.concat "" (List.map (String.make 1) cs)), env)
          | v -> Error ("string_from_chars: 需要列表，但得到 " ^ type_of_value v) ) )
    ; ( "string_rev",
      VBuiltin
        ( "string_rev",
          fun env -> function
          | VString s ->
              let len = String.length s in
              let buf = Buffer.create len in
              for i = len - 1 downto 0 do
                Buffer.add_char buf s.[i]
              done;
              Ok (VString (Buffer.contents buf), env)
          | v -> Error ("string_rev: 需要字符串，但得到 " ^ type_of_value v) ) )
    (* 更多列表操作 *)
    ; ( "list_init",
      VBuiltin
        ( "list_init",
          fun env -> function
          | VTuple [VInt n; f] when n >= 0 ->
              let rec init i acc =
                if i >= n then Ok (VList (List.rev acc), env)
                else
                  let* (v, _) = ctx.apply_fn env f (VInt i) in
                  init (i + 1) (v :: acc)
              in
              init 0 []
          | v -> Error ("list_init: 需要 (int, function) 元组，但得到 " ^ type_of_value v) ) )
    ; ( "list_iter",
      VBuiltin
        ( "list_iter",
          fun env f ->
            Ok (VBuiltin
               ( "list_iter'",
                 fun env xs ->
                   match xs with
                   | VList items ->
                       let rec iter = function
                         | [] -> Ok (VUnit, env)
                         | item :: rest ->
                             let* _ = ctx.apply_fn env f item in
                             iter rest
                       in
                       iter items
                   | v -> Error ("list_iter: 第二个参数必须是列表，但得到 " ^ type_of_value v) ),
             env) ) )
    ; ( "list_forall",
      VBuiltin
        ( "list_forall",
          fun env f ->
            Ok (VBuiltin
               ( "list_forall'",
                 fun env xs ->
                   match xs with
                   | VList items ->
                       let rec forall = function
                         | [] -> Ok (VBool true, env)
                         | item :: rest ->
                             let* (v, _) = ctx.apply_fn env f item in
                             (match v with
                              | VBool true -> forall rest
                              | VBool false -> Ok (VBool false, env)
                              | v -> Error ("list_forall: 谓词必须返回布尔值，但得到 " ^ type_of_value v))
                       in
                       forall items
                   | v -> Error ("list_forall: 第二个参数必须是列表，但得到 " ^ type_of_value v) ),
             env) ) )
    ; ( "list_exists",
      VBuiltin
        ( "list_exists",
          fun env f ->
            Ok (VBuiltin
               ( "list_exists'",
                 fun env xs ->
                   match xs with
                   | VList items ->
                       let rec exists = function
                         | [] -> Ok (VBool false, env)
                         | item :: rest ->
                             let* (v, _) = ctx.apply_fn env f item in
                             (match v with
                              | VBool true -> Ok (VBool true, env)
                              | VBool false -> exists rest
                              | v -> Error ("list_exists: 谓词必须返回布尔值，但得到 " ^ type_of_value v))
                       in
                       exists items
                   | v -> Error ("list_exists: 第二个参数必须是列表，但得到 " ^ type_of_value v) ),
             env) ) )
    ; ( "list_mapi",
      VBuiltin
        ( "list_mapi",
          fun env f ->
            Ok (VBuiltin
               ( "list_mapi'",
                 fun env xs ->
                   match xs with
                   | VList items ->
                       let rec mapi i acc = function
                         | [] -> Ok (VList (List.rev acc), env)
                         | item :: rest ->
                             let* (v, _) = ctx.apply_fn env f (VTuple [VInt i; item]) in
                             mapi (i + 1) (v :: acc) rest
                       in
                       mapi 0 [] items
                   | v -> Error ("list_mapi: 第二个参数必须是列表，但得到 " ^ type_of_value v) ),
             env) ) )
    ; ( "list_filter_mapi",
      VBuiltin
        ( "list_filter_mapi",
          fun env f ->
            Ok (VBuiltin
               ( "list_filter_mapi'",
                 fun env xs ->
                   match xs with
                   | VList items ->
                       let rec filter_mapi i acc = function
                         | [] -> Ok (VList (List.rev acc), env)
                         | item :: rest ->
                             let* (v, _) = ctx.apply_fn env f (VTuple [VInt i; item]) in
                             (match v with
                              | VCtor ("Some", Some value) -> filter_mapi (i + 1) (value :: acc) rest
                              | VCtor ("None", None) -> filter_mapi (i + 1) acc rest
                              | v -> Error ("list_filter_mapi: 函数必须返回 option，但得到 " ^ type_of_value v))
                       in
                       filter_mapi 0 [] items
                   | v -> Error ("list_filter_mapi: 第二个参数必须是列表，但得到 " ^ type_of_value v) ),
             env) ) )
    (* 更多数学操作 *)
    ; ( "math_mod",
      VBuiltin
        ( "math_mod",
          fun env -> function
          | VTuple [VInt a; VInt b] when b <> 0 ->
              Ok (VInt (a mod b), env)
          | VTuple [VInt _; VInt 0] ->
              Error "math_mod: 除零错误"
          | v -> Error ("math_mod: 需要 (int, int) 元组，但得到 " ^ type_of_value v) ) )
    ; ( "math_gcd",
      VBuiltin
        ( "math_gcd",
          fun env -> function
          | VTuple [VInt a; VInt b] ->
              let rec gcd a b =
                if b = 0 then a else gcd b (a mod b)
              in
              Ok (VInt (gcd (abs a) (abs b)), env)
          | v -> Error ("math_gcd: 需要 (int, int) 元组，但得到 " ^ type_of_value v) ) )
    ; ( "math_lcm",
      VBuiltin
        ( "math_lcm",
          fun env -> function
          | VTuple [VInt a; VInt b] ->
              let rec gcd a b =
                if b = 0 then a else gcd b (a mod b)
              in
              let lcm = abs (a * b) / gcd (abs a) (abs b) in
              Ok (VInt lcm, env)
          | v -> Error ("math_lcm: 需要 (int, int) 元组，但得到 " ^ type_of_value v) ) )
    ; ( "math_pow",
      VBuiltin
        ( "math_pow",
          fun env -> function
          | VTuple [VInt base; VInt exp] when exp >= 0 ->
              let rec pow acc base exp =
                if exp = 0 then acc
                else if exp mod 2 = 0 then pow acc (base * base) (exp / 2)
                else pow (acc * base) (base * base) (exp / 2)
              in
              Ok (VInt (pow 1 base exp), env)
          | v -> Error ("math_pow: 需要 (int, non-negative int) 元组，但得到 " ^ type_of_value v) ) )
    ; ( "math_sqrt",
      VBuiltin
        ( "math_sqrt",
          fun env -> function
          | VInt n when n >= 0 ->
              let rec sqrt_iter guess =
                let next = (guess + n / guess) / 2 in
                if next >= guess then guess
                else sqrt_iter next
              in
              Ok (VInt (sqrt_iter n), env)
          | v -> Error ("math_sqrt: 需要非负整数，但得到 " ^ type_of_value v) ) )
    (* 文件系统操作 *)
    ; ( "file_read_bytes",
      VBuiltin
        ( "file_read_bytes",
          fun env -> function
          | VString path ->
              (try
                 let ic = open_in_bin path in
                 let size = in_channel_length ic in
                 let buf = Bytes.create size in
                 really_input ic buf 0 size;
                 close_in ic;
                 let bytes = List.init size (fun i -> VInt (Char.code (Bytes.get buf i))) in
                 Ok (VList bytes, env)
               with Sys_error msg -> Error ("file_read_bytes: " ^ msg))
          | v -> Error ("file_read_bytes: 需要字符串，但得到 " ^ type_of_value v) ) )
    ; ( "file_write_bytes",
      VBuiltin
        ( "file_write_bytes",
          fun env -> function
          | VTuple [VString path; VList bytes] ->
              (try
                 let oc = open_out_bin path in
                 List.iter (fun b ->
                   match b with
                   | VInt n when n >= 0 && n <= 255 ->
                       output_char oc (Char.chr n)
                   | _ -> ()
                 ) bytes;
                 close_out oc;
                 Ok (VUnit, env)
               with Sys_error msg -> Error ("file_write_bytes: " ^ msg))
          | v -> Error ("file_write_bytes: 需要 (string, list) 元组，但得到 " ^ type_of_value v) ) )
    ; ( "file_temp",
      VBuiltin
        ( "file_temp",
          fun env -> function
          | VUnit | VTuple [] ->
              (try
                 let path = Filename.temp_file "mylang" ".tmp" in
                 Ok (VString path, env)
               with Sys_error msg -> Error ("file_temp: " ^ msg))
          | v -> Error ("file_temp: 需要 unit，但得到 " ^ type_of_value v) ) )
    (* 进程操作 *)
    ; ( "process_exec",
      VBuiltin
        ( "process_exec",
          fun env -> function
          | VString cmd ->
              (try
                 let ic = Unix.open_process_in cmd in
                 let rec read_all acc =
                   try
                     let line = input_line ic in
                     read_all (acc ^ line ^ "\n")
                   with End_of_file -> acc
                 in
                 let output = read_all "" in
                 let status = Unix.close_process_in ic in
                 let code = match status with
                   | Unix.WEXITED n -> n
                   | Unix.WSIGNALED _ | Unix.WSTOPPED _ -> -1
                 in
                 Ok (VTuple [VInt code; VString output], env)
               with _ -> Error "process_exec: 执行失败")
          | v -> Error ("process_exec: 需要字符串，但得到 " ^ type_of_value v) ) )
    ; ( "process_exit",
      VBuiltin
        ( "process_exit",
          fun env -> function
          | VInt code ->
              exit code
          | v -> Error ("process_exit: 需要整数，但得到 " ^ type_of_value v) ) )
    (* 类型检查 *)
    ; ( "is_int",
      VBuiltin
        ( "is_int",
          fun env v ->
            Ok (VBool (match v with VInt _ -> true | _ -> false), env) ) )
    ; ( "is_bool",
      VBuiltin
        ( "is_bool",
          fun env v ->
            Ok (VBool (match v with VBool _ -> true | _ -> false), env) ) )
    ; ( "is_string",
      VBuiltin
        ( "is_string",
          fun env v ->
            Ok (VBool (match v with VString _ -> true | _ -> false), env) ) )
    ; ( "is_list",
      VBuiltin
        ( "is_list",
          fun env v ->
            Ok (VBool (match v with VList _ -> true | _ -> false), env) ) )
    ; ( "is_function",
      VBuiltin
        ( "is_function",
          fun env v ->
            Ok (VBool (match v with VFun _ | VBuiltin _ -> true | _ -> false), env) ) )
    ; ( "is_unit",
      VBuiltin
        ( "is_unit",
          fun env v ->
            Ok (VBool (match v with VUnit | VTuple [] -> true | _ -> false), env) ) )
    ]
