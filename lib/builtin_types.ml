(** 内置函数类型环境

    将标准库函数的类型签名集中管理，与求值逻辑解耦。
*)

open Types

(** 内置类型环境：函数名到类型方案的映射 *)
let builtin_type_env =
  [ ( "head",
      Forall
        ( [0],
          TArrow (TList (TVar 0), TVar 0) ) )
  ; ( "tail",
      Forall
        ( [0],
          TArrow (TList (TVar 0), TList (TVar 0)) ) )
  ; ( "length",
      Forall
        ( [0],
          TArrow (TList (TVar 0), TInt) ) )
  ; ( "print",
      Forall
        ( [0],
          TArrow (TVar 0, TUnit) ) )
  ; ( "import",
      Forall
        ( [0],
          TArrow (TString, TUnit) ) )
  ; ( "show",
      Forall
        ( [0],
          TArrow (TVar 0, TString) ) )
  ; ( "string_length",
      Forall
        ( [],
          TArrow (TString, TInt) ) )
  ; ( "string_get",
      Forall
        ( [],
          TArrow (TString, TArrow (TInt, TChar)) ) )
  ; ( "string_sub",
      Forall
        ( [],
          TArrow (TString, TArrow (TInt, TArrow (TInt, TString))) ) )
  ; ( "read_file",
      Forall
        ( [],
          TArrow (TString, TString) ) )
  ; ( "write_file",
      Forall
        ( [],
          TArrow (TString, TArrow (TString, TUnit)) ) )
  ; ( "read_line",
      Forall
        ( [],
          TArrow (TUnit, TString) ) )
  ; ( "print_string",
      Forall
        ( [],
          TArrow (TString, TUnit) ) )
  ; ( "map",
      Forall
        ( [0; 1],
          TArrow
            ( TArrow (TVar 0, TVar 1),
              TArrow (TList (TVar 0), TList (TVar 1)) ) ) )
  ; ( "filter",
      Forall
        ( [0],
          TArrow
            ( TArrow (TVar 0, TBool),
              TArrow (TList (TVar 0), TList (TVar 0)) ) ) )
  ; ( "fold",
      Forall
        ( [0; 1],
          TArrow
            ( TArrow (TVar 1, TArrow (TVar 0, TVar 1)),
              TArrow (TVar 1, TArrow (TList (TVar 0), TVar 1)) ) ) )
  ; ( "range",
      Forall
        ( [],
          TArrow
            ( TInt,
              TArrow (TInt, TList TInt) ) ) )
  ; ( "sum",
      Forall
        ( [],
          TArrow (TList TInt, TInt) ) )
  ; ( "reverse",
      Forall
        ( [0],
          TArrow (TList (TVar 0), TList (TVar 0)) ) )
  ; ( "append",
      Forall
        ( [0],
          TArrow
            ( TList (TVar 0),
              TArrow (TList (TVar 0), TList (TVar 0)) ) ) )
  ; ( "timeit",
      Forall
        ( [0],
          TArrow
            ( TArrow (TUnit, TVar 0),
              TVar 0 ) ) )
  ; ( "string_trim",
      Forall ([], TArrow (TString, TString)) )
  ; ( "string_uppercase",
      Forall ([], TArrow (TString, TString)) )
  ; ( "string_lowercase",
      Forall ([], TArrow (TString, TString)) )
  ; ( "string_concat",
      Forall ([], TArrow (TTuple [TString; TList TString], TString)) )
  ; ( "string_split",
      Forall ([], TArrow (TTuple [TString; TString], TList TString)) )
  ; ( "string_contains",
      Forall ([], TArrow (TTuple [TString; TString], TBool)) )
  ; ( "string_replace",
      Forall ([], TArrow (TTuple [TString; TString; TString], TString)) )
  ; ( "take",
      Forall ([0], TArrow (TTuple [TInt; TList (TVar 0)], TList (TVar 0))) )
  ; ( "drop",
      Forall ([0], TArrow (TTuple [TInt; TList (TVar 0)], TList (TVar 0))) )
  ; ( "find",
      Forall ([0], TArrow (TTuple [TArrow (TVar 0, TBool); TList (TVar 0)], TADT ("option", [TVar 0]))) )
  ; ( "exists",
      Forall ([0], TArrow (TTuple [TArrow (TVar 0, TBool); TList (TVar 0)], TBool)) )
  ; ( "forall",
      Forall ([0], TArrow (TTuple [TArrow (TVar 0, TBool); TList (TVar 0)], TBool)) )
  ; ( "sort",
      Forall ([0], TArrow (TList (TVar 0), TList (TVar 0))) )
  ; ( "zip",
      Forall ([0; 1], TArrow (TTuple [TList (TVar 0); TList (TVar 1)], TList (TTuple [TVar 0; TVar 1]))) )
  ; ( "abs",
      Forall ([], TArrow (TInt, TInt)) )
  ; ( "min",
      Forall ([0], TArrow (TTuple [TVar 0; TVar 0], TVar 0)) )
  ; ( "max",
      Forall ([0], TArrow (TTuple [TVar 0; TVar 0], TVar 0)) )
  ; ( "int_of_string",
      Forall ([], TArrow (TString, TInt)) )
  ; ( "string_of_int",
      Forall ([], TArrow (TInt, TString)) )
  ; ( "int_of_char",
      Forall ([], TArrow (TChar, TInt)) )
  ; ( "char_of_int",
      Forall ([], TArrow (TInt, TChar)) )
  ; ( "sqrt",
      Forall ([], TArrow (TInt, TInt)) )
  ; ( "pow",
      Forall ([], TArrow (TTuple [TInt; TInt], TInt)) )
  ; ( "random_int",
      Forall ([], TArrow (TTuple [TInt; TInt], TInt)) )
  ; ( "current_time",
      Forall ([], TArrow (TUnit, TInt)) )
  ; ( "sleep",
      Forall ([], TArrow (TInt, TUnit)) )
  ; ( "file_exists",
      Forall ([], TArrow (TString, TBool)) )
  ; ( "file_size",
      Forall ([], TArrow (TString, TInt)) )
  ; ( "delete_file",
      Forall ([], TArrow (TString, TUnit)) )
  ; ( "list_directory",
      Forall ([], TArrow (TString, TList TString)) )
  ; ( "get_env",
      Forall ([], TArrow (TString, TADT ("option", [TString]))) )
  ; ( "system_command",
      Forall ([], TArrow (TString, TInt)) )
  ; ( "regex_match",
      Forall ([], TArrow (TTuple [TString; TString], TBool)) )
  ; ( "regex_replace",
      Forall ([], TArrow (TTuple [TString; TString; TString], TString)) )
  ; ( "regex_split",
      Forall ([], TArrow (TTuple [TString; TString], TList TString)) )
    (* 新增标准库类型 *)
  ; ( "hashmap_create",
      Forall ([], TArrow (TUnit, TRecord [])) )
  ; ( "hashmap_get",
      Forall ([0], TArrow (TTuple [TRecord []; TString], TADT ("option", [TVar 0]))) )
  ; ( "hashmap_set",
      Forall ([0], TArrow (TTuple [TRecord []; TString; TVar 0], TRecord [])) )
  ; ( "hashmap_delete",
      Forall ([], TArrow (TTuple [TRecord []; TString], TRecord [])) )
  ; ( "hashmap_keys",
      Forall ([], TArrow (TRecord [], TList TString)) )
  ; ( "hashmap_values",
      Forall ([0], TArrow (TRecord [], TList (TVar 0))) )
  ; ( "hashmap_size",
      Forall ([], TArrow (TRecord [], TInt)) )
  ; ( "hashmap_has_key",
      Forall ([], TArrow (TTuple [TRecord []; TString], TBool)) )
  ; ( "read_lines",
      Forall ([], TArrow (TString, TList TString)) )
  ; ( "write_lines",
      Forall ([], TArrow (TTuple [TString; TList TString], TUnit)) )
  ; ( "append_file",
      Forall ([], TArrow (TTuple [TString; TString], TUnit)) )
  ; ( "copy_file",
      Forall ([], TArrow (TTuple [TString; TString], TUnit)) )
  ; ( "string_starts_with",
      Forall ([], TArrow (TTuple [TString; TString], TBool)) )
  ; ( "string_ends_with",
      Forall ([], TArrow (TTuple [TString; TString], TBool)) )
  ; ( "string_repeat",
      Forall ([], TArrow (TTuple [TString; TInt], TString)) )
  ; ( "string_pad_left",
      Forall ([], TArrow (TTuple [TString; TInt; TString], TString)) )
  ; ( "string_pad_right",
      Forall ([], TArrow (TTuple [TString; TInt; TString], TString)) )
  ; ( "list_flatten",
      Forall ([0], TArrow (TList (TList (TVar 0)), TList (TVar 0))) )
  ; ( "list_flat_map",
      Forall ([0; 1], TArrow (TTuple [TArrow (TVar 0, TList (TVar 1)); TList (TVar 0)], TList (TVar 1))) )
  ; ( "list_count",
      Forall ([0], TArrow (TTuple [TArrow (TVar 0, TBool); TList (TVar 0)], TInt)) )
  ; ( "list_distinct",
      Forall ([0], TArrow (TList (TVar 0), TList (TVar 0))) )
  ; ( "list_group_by",
      Forall ([0], TArrow (TTuple [TArrow (TVar 0, TString); TList (TVar 0)], TRecord [])) )
  ; ( "math_abs",
      Forall ([], TArrow (TInt, TInt)) )
  ; ( "math_min",
      Forall ([], TArrow (TTuple [TInt; TInt], TInt)) )
  ; ( "math_max",
      Forall ([], TArrow (TTuple [TInt; TInt], TInt)) )
  ; ( "math_clamp",
      Forall ([], TArrow (TTuple [TInt; TInt; TInt], TInt)) )
  ; ( "math_sum",
      Forall ([], TArrow (TList TInt, TInt)) )
  ; ( "math_product",
      Forall ([], TArrow (TList TInt, TInt)) )
  ; ( "int_to_string",
      Forall ([], TArrow (TInt, TString)) )
  ; ( "string_to_int",
      Forall ([], TArrow (TString, TInt)) )
  ; ( "bool_to_string",
      Forall ([], TArrow (TBool, TString)) )
  ; ( "char_to_string",
      Forall ([], TArrow (TChar, TString)) )
  ; ( "debug_print",
      Forall ([0], TArrow (TVar 0, TUnit)) )
  ; ( "debug_to_string",
      Forall ([0], TArrow (TVar 0, TString)) )
    (* JSON 支持 *)
  ; ( "json_parse",
      Forall ([], TArrow (TString, TRecord [])) )
  ; ( "json_stringify",
      Forall ([0], TArrow (TVar 0, TString)) )
  ; ( "json_pretty",
      Forall ([0], TArrow (TVar 0, TString)) )
    (* 日期时间 *)
  ; ( "time_now",
      Forall ([], TArrow (TUnit, TInt)) )
  ; ( "time_now_ms",
      Forall ([], TArrow (TUnit, TInt)) )
  ; ( "time_sleep_ms",
      Forall ([], TArrow (TInt, TUnit)) )
  ; ( "time_format",
      Forall ([], TArrow (TTuple [TInt; TString], TString)) )
  ; ( "time_year",
      Forall ([], TArrow (TInt, TInt)) )
  ; ( "time_month",
      Forall ([], TArrow (TInt, TInt)) )
  ; ( "time_day",
      Forall ([], TArrow (TInt, TInt)) )
  ; ( "time_hour",
      Forall ([], TArrow (TInt, TInt)) )
  ; ( "time_minute",
      Forall ([], TArrow (TInt, TInt)) )
  ; ( "time_second",
      Forall ([], TArrow (TInt, TInt)) )
  ; ( "time_day_of_week",
      Forall ([], TArrow (TInt, TInt)) )
    (* 集合操作 *)
  ; ( "set_create",
      Forall ([0], TArrow (TUnit, TList (TVar 0))) )
  ; ( "set_add",
      Forall ([0], TArrow (TTuple [TList (TVar 0); TVar 0], TList (TVar 0))) )
  ; ( "set_remove",
      Forall ([0], TArrow (TTuple [TList (TVar 0); TVar 0], TList (TVar 0))) )
  ; ( "set_contains",
      Forall ([0], TArrow (TTuple [TList (TVar 0); TVar 0], TBool)) )
  ; ( "set_size",
      Forall ([0], TArrow (TList (TVar 0), TInt)) )
  ; ( "set_union",
      Forall ([0], TArrow (TTuple [TList (TVar 0); TList (TVar 0)], TList (TVar 0))) )
  ; ( "set_intersection",
      Forall ([0], TArrow (TTuple [TList (TVar 0); TList (TVar 0)], TList (TVar 0))) )
  ; ( "set_difference",
      Forall ([0], TArrow (TTuple [TList (TVar 0); TList (TVar 0)], TList (TVar 0))) )
    (* 网络操作 *)
  ; ( "http_get",
      Forall ([], TArrow (TString, TString)) )
  ; ( "http_post",
      Forall ([], TArrow (TTuple [TString; TString], TString)) )
  ; ( "url_encode",
      Forall ([], TArrow (TString, TString)) )
  ; ( "url_decode",
      Forall ([], TArrow (TString, TString)) )
    (* 加密操作 *)
  ; ( "hash_md5",
      Forall ([], TArrow (TString, TString)) )
  ; ( "hash_sha256",
      Forall ([], TArrow (TString, TString)) )
  ; ( "base64_encode",
      Forall ([], TArrow (TString, TString)) )
  ; ( "base64_decode",
      Forall ([], TArrow (TString, TString)) )
  ; ( "hex_encode",
      Forall ([], TArrow (TString, TString)) )
  ; ( "hex_decode",
      Forall ([], TArrow (TString, TString)) )
    (* 并发操作 *)
  ; ( "thread_create",
      Forall ([0], TArrow (TArrow (TUnit, TVar 0), TInt)) )
  ; ( "thread_join",
      Forall ([], TArrow (TInt, TUnit)) )
  ; ( "mutex_create",
      Forall ([], TArrow (TUnit, TInt)) )
  ; ( "mutex_lock",
      Forall ([], TArrow (TInt, TUnit)) )
  ; ( "mutex_unlock",
      Forall ([], TArrow (TInt, TUnit)) )
  ; ( "channel_create",
      Forall ([], TArrow (TUnit, TRecord [])) )
  ; ( "channel_send",
      Forall ([0], TArrow (TTuple [TRecord []; TVar 0], TUnit)) )
  ; ( "channel_receive",
      Forall ([0], TArrow (TRecord [], TVar 0)) )
    (* 调试增强 *)
  ; ( "debug_trace",
      Forall ([0], TArrow (TVar 0, TVar 0)) )
  ; ( "debug_assert",
      Forall ([], TArrow (TBool, TUnit)) )
  ; ( "debug_type",
      Forall ([0], TArrow (TVar 0, TString)) )
    (* 工业级标准库扩充 *)
  ; ( "string_join",
      Forall ([], TArrow (TTuple [TString; TList TString], TString)) )
  ; ( "string_to_chars",
      Forall ([], TArrow (TString, TList TChar)) )
  ; ( "string_from_chars",
      Forall ([], TArrow (TList TChar, TString)) )
  ; ( "string_rev",
      Forall ([], TArrow (TString, TString)) )
  ; ( "list_init",
      Forall ([0], TArrow (TTuple [TInt; TArrow (TInt, TVar 0)], TList (TVar 0))) )
  ; ( "list_iter",
      Forall ([0], TArrow (TArrow (TVar 0, TUnit), TArrow (TList (TVar 0), TUnit))) )
  ; ( "list_forall",
      Forall ([0], TArrow (TArrow (TVar 0, TBool), TArrow (TList (TVar 0), TBool))) )
  ; ( "list_exists",
      Forall ([0], TArrow (TArrow (TVar 0, TBool), TArrow (TList (TVar 0), TBool))) )
  ; ( "list_mapi",
      Forall ([0; 1], TArrow (TArrow (TTuple [TInt; TVar 0], TVar 1), TArrow (TList (TVar 0), TList (TVar 1)))) )
  ; ( "list_filter_mapi",
      Forall ([0; 1], TArrow (TArrow (TTuple [TInt; TVar 0], TADT ("option", [TVar 1])), TArrow (TList (TVar 0), TList (TVar 1)))) )
  ; ( "math_mod",
      Forall ([], TArrow (TTuple [TInt; TInt], TInt)) )
  ; ( "math_gcd",
      Forall ([], TArrow (TTuple [TInt; TInt], TInt)) )
  ; ( "math_lcm",
      Forall ([], TArrow (TTuple [TInt; TInt], TInt)) )
  ; ( "math_pow",
      Forall ([], TArrow (TTuple [TInt; TInt], TInt)) )
  ; ( "math_sqrt",
      Forall ([], TArrow (TInt, TInt)) )
  ; ( "file_read_bytes",
      Forall ([], TArrow (TString, TList TInt)) )
  ; ( "file_write_bytes",
      Forall ([], TArrow (TTuple [TString; TList TInt], TUnit)) )
  ; ( "file_temp",
      Forall ([], TArrow (TUnit, TString)) )
  ; ( "process_exec",
      Forall ([], TArrow (TString, TTuple [TInt; TString])) )
  ; ( "process_exit",
      Forall ([], TArrow (TInt, TUnit)) )
  ; ( "is_int",
      Forall ([0], TArrow (TVar 0, TBool)) )
  ; ( "is_bool",
      Forall ([0], TArrow (TVar 0, TBool)) )
  ; ( "is_string",
      Forall ([0], TArrow (TVar 0, TBool)) )
  ; ( "is_list",
      Forall ([0], TArrow (TVar 0, TBool)) )
  ; ( "is_function",
      Forall ([0], TArrow (TVar 0, TBool)) )
  ; ( "is_unit",
      Forall ([0], TArrow (TVar 0, TBool)) )
    (* 工业级标准库扩充 *)
  ; ( "string_trim_left",
      Forall ([], TArrow (TString, TString)) )
  ; ( "string_trim_right",
      Forall ([], TArrow (TString, TString)) )
  ; ( "string_contains",
      Forall ([], TArrow (TTuple [TString; TString], TBool)) )
  ; ( "string_starts_with",
      Forall ([], TArrow (TTuple [TString; TString], TBool)) )
  ; ( "string_ends_with",
      Forall ([], TArrow (TTuple [TString; TString], TBool)) )
  ; ( "string_find",
      Forall ([], TArrow (TTuple [TString; TString], TInt)) )
  ; ( "string_count",
      Forall ([], TArrow (TTuple [TString; TString], TInt)) )
  ; ( "list_take_while",
      Forall ([0], TArrow (TArrow (TVar 0, TBool), TArrow (TList (TVar 0), TList (TVar 0)))) )
  ; ( "list_drop_while",
      Forall ([0], TArrow (TArrow (TVar 0, TBool), TArrow (TList (TVar 0), TList (TVar 0)))) )
  ; ( "list_partition",
      Forall ([0], TArrow (TArrow (TVar 0, TBool), TArrow (TList (TVar 0), TTuple [TList (TVar 0); TList (TVar 0)]))) )
  ; ( "list_scan",
      Forall ([0; 1], TArrow (TArrow (TTuple [TVar 1; TVar 0], TVar 1), TArrow (TVar 1, TArrow (TList (TVar 0), TList (TVar 1))))) )
  ; ( "list_zip_with",
      Forall ([0; 1; 2], TArrow (TArrow (TTuple [TVar 0; TVar 1], TVar 2), TArrow (TList (TVar 0), TArrow (TList (TVar 1), TList (TVar 2))))) )
  ; ( "math_abs",
      Forall ([], TArrow (TInt, TInt)) )
  ; ( "math_sign",
      Forall ([], TArrow (TInt, TInt)) )
  ; ( "math_clamp",
      Forall ([], TArrow (TTuple [TInt; TInt; TInt], TInt)) )
  ; ( "math_min",
      Forall ([], TArrow (TTuple [TInt; TInt], TInt)) )
  ; ( "math_max",
      Forall ([], TArrow (TTuple [TInt; TInt], TInt)) )
  ; ( "file_read_lines",
      Forall ([], TArrow (TString, TList TString)) )
  ; ( "file_write_lines",
      Forall ([], TArrow (TTuple [TString; TList TString], TUnit)) )
  ; ( "file_append",
      Forall ([], TArrow (TTuple [TString; TString], TUnit)) )
  ; ( "file_copy",
      Forall ([], TArrow (TTuple [TString; TString], TUnit)) )
  ; ( "process_get_env",
      Forall ([], TArrow (TString, TString)) )
  ; ( "process_set_env",
      Forall ([], TArrow (TTuple [TString; TString], TUnit)) )
  ; ( "process_cwd",
      Forall ([], TArrow (TUnit, TString)) )
  ; ( "process_chdir",
      Forall ([], TArrow (TString, TUnit)) )
  ]
