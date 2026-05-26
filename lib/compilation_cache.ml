(** 编译缓存

    管理编译产物的缓存，基于文件内容哈希决定是否重新编译。
    缓存存储在 .my_lang_cache/ 目录下。
*)

open Core

let cache_dir = ".my_lang_cache"

(** 确保缓存目录存在 *)
let ensure_cache_dir () =
  try
    if not (Stdlib.Sys.file_exists cache_dir) then
      Stdlib.Sys.mkdir cache_dir 0o755
  with _ -> ()

(** 计算文件内容的 SHA256 哈希 *)
let compute_hash content =
  Md5.digest_string content
  |> Md5.to_hex

(** 编译产物 *)
type compilation_artifact = {
  source_hash : string;
  bytecode : string;  (** 序列化的字节码 *)
  timestamp : float;
}

(** 获取缓存文件路径 *)
let cache_file_path module_name =
  Filename.concat cache_dir (module_name ^ ".cache")

(** 保存编译产物到缓存 *)
let save_artifact module_name artifact =
  ensure_cache_dir ();
  let path = cache_file_path module_name in
  let data =
    Printf.sprintf "%s\n%f\n%s"
      artifact.source_hash
      artifact.timestamp
      artifact.bytecode
  in
  Out_channel.write_all path ~data

(** 从缓存读取编译产物 *)
let load_artifact module_name =
  let path = cache_file_path module_name in
  if not (Stdlib.Sys.file_exists path) then None
  else
    try
      let content = In_channel.read_all path in
      match String.split_lines content with
      | hash_str :: timestamp_str :: bytecode_lines ->
          Some {
            source_hash = hash_str;
            timestamp = Float.of_string timestamp_str;
            bytecode = String.concat ~sep:"\n" bytecode_lines;
          }
      | _ -> None
    with _ -> None

(** 检查是否需要重新编译 *)
let needs_recompile module_name source_content =
  let current_hash = compute_hash source_content in
  match load_artifact module_name with
  | None -> true  (* 没有缓存 *)
  | Some artifact -> not (String.equal artifact.source_hash current_hash)  (* 哈希不匹配 *)

(** 删除缓存 *)
let invalidate_cache module_name =
  let path = cache_file_path module_name in
  if Stdlib.Sys.file_exists path then
    Stdlib.Sys.remove path

(** AST 缓存文件路径 *)
let ast_cache_path module_name =
  Filename.concat cache_dir (module_name ^ ".ast")

(** 保存 AST 到缓存 *)
let save_ast_cache module_name source_hash expr =
  ensure_cache_dir ();
  let path = ast_cache_path module_name in
  let data = Marshal.to_string expr [] in
  let oc = Stdlib.open_out_bin path in
  Stdlib.output_string oc (source_hash ^ "\n");
  Stdlib.output_string oc data;
  Stdlib.close_out oc

(** 从缓存加载 AST *)
let load_ast_cache module_name source_hash =
  let path = ast_cache_path module_name in
  if not (Stdlib.Sys.file_exists path) then None
  else
    try
      let ic = Stdlib.open_in_bin path in
      let cached_hash = Stdlib.input_line ic in
      if not (String.equal cached_hash source_hash) then begin
        Stdlib.close_in ic;
        None
      end else begin
        let data = Stdlib.really_input_string ic (Stdlib.in_channel_length ic - String.length cached_hash - 1) in
        Stdlib.close_in ic;
        Some (Marshal.from_string data 0)
      end
    with _ -> None

(** 删除 AST 缓存 *)
let invalidate_ast_cache module_name =
  let path = ast_cache_path module_name in
  if Stdlib.Sys.file_exists path then
    Stdlib.Sys.remove path

(** 清除所有缓存 *)
let clear_all_cache () =
  if Stdlib.Sys.file_exists cache_dir then
    let files = Stdlib.Sys.readdir cache_dir in
    Array.iter files ~f:(fun f ->
      Stdlib.Sys.remove (Filename.concat cache_dir f))