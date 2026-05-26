(** 资源管理器

    提供安全的资源获取和释放机制，避免临时文件和内存泄漏。
*)

open Core

(** 临时文件资源 *)
type temp_resource = {
  path : string;
  mutable cleaned : bool;
}

let create_temp_file prefix suffix =
  let path = "/tmp/" ^ prefix ^ "_" ^ string_of_int (Random.int 100000) ^ suffix in
  { path; cleaned = false }

let cleanup_resource r =
  if not r.cleaned then (
    try Stdlib.Sys.remove r.path with _ -> ();
    r.cleaned <- true
  )

(** 带自动清理的执行上下文 *)
let with_temp_file prefix suffix f =
  let resource = create_temp_file prefix suffix in
  try
    let result = f resource.path in
    cleanup_resource resource;
    result
  with exn ->
    cleanup_resource resource;
    raise exn

(** 批量临时文件管理 *)
let with_temp_files files f =
  let resources = List.map files ~f:(fun (prefix, suffix) -> create_temp_file prefix suffix) in
  try
    let paths = List.map resources ~f:(fun r -> r.path) in
    let result = f paths in
    List.iter resources ~f:cleanup_resource;
    result
  with exn ->
    List.iter resources ~f:cleanup_resource;
    raise exn

(** 运行命令并自动清理输出文件 *)
let run_command_with_cleanup cmd =
  with_temp_files [("my_lang_stdout", ""); ("my_lang_stderr", "")] (fun paths ->
    match paths with
    | [stdout_file; stderr_file] ->
        let full_cmd = Printf.sprintf "%s > %s 2> %s" cmd stdout_file stderr_file in
        let exit_code = Stdlib.Sys.command full_cmd in
        let stdout = In_channel.read_all stdout_file in
        let stderr = In_channel.read_all stderr_file in
        (exit_code, stdout, stderr)
    | _ -> failwith "Internal error: expected exactly 2 temp files"
  )
