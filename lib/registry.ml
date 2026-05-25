(** 包注册表

    基于 Git 的包注册表原型。
    注册表格式：
    - packages/
      - package-name/
        - versions/
          - 1.0.0/
            - my-lang.toml
            - src/
        - metadata.json
*)

open Core

(** 包元数据 *)
type package_metadata = {
  name : string;
  description : string;
  author : string;
  repository : string;
  versions : string list;
}

(** 注册表配置 *)
type registry_config = {
  url : string;  (* 注册表 Git 仓库 URL *)
  local_path : string;  (* 本地缓存路径 *)
}

(** 默认本地缓存路径 *)
let default_registry_path = "~/.my-lang/registry"

(** 确保注册表目录存在 *)
let ensure_registry_dir () =
  let path = default_registry_path in
  try
    let _ = Stdlib.Sys.readdir path in
    ()
  with Sys_error _ ->
    Stdlib.Sys.mkdir path 0o755

(** 从 Git 仓库克隆/更新注册表 *)
let update_registry url =
  let path = default_registry_path in
  if Stdlib.Sys.file_exists (Filename.concat path ".git") then
    (* 更新已有仓库 *)
    Printf.printf "Updating registry from %s...\n" url
  else
    (* 克隆新仓库 *)
    Printf.printf "Cloning registry from %s...\n" url;
    Stdlib.Sys.mkdir path 0o755

(** 搜索包 *)
let search_packages query =
  Printf.printf "Searching for '%s'...\n" query;
  (* 模拟搜索结果 *)
  [
    { name = "stdlib"; description = "MyLang standard library"; author = "my-lang"; repository = "https://github.com/my-lang/stdlib"; versions = ["1.0.0"; "0.9.0"] };
    { name = "json"; description = "JSON parsing and generation"; author = "community"; repository = "https://github.com/my-lang/json"; versions = ["0.1.0"] };
    { name = "http"; description = "HTTP client/server"; author = "community"; repository = "https://github.com/my-lang/http"; versions = ["0.1.0"] };
  ]

(** 获取包的最新版本 *)
let get_latest_version metadata =
  match metadata.versions with
  | [] -> None
  | v :: _ -> Some v

(** 安装包 *)
let install_package name version =
  Printf.printf "Installing %s@%s...\n" name version;
  (* 模拟安装过程 *)
  let install_dir = Printf.sprintf "deps/%s" name in
  (try Stdlib.Sys.mkdir "deps" 0o755 with Sys_error _ -> ());
  (try Stdlib.Sys.mkdir install_dir 0o755 with Sys_error _ -> ());
  
  (* 创建简单的占位文件 *)
  Out_channel.write_all (Filename.concat install_dir ".installed") ~data:(Printf.sprintf "%s\n" version);
  Printf.printf "✓ %s@%s installed to %s\n" name version install_dir

(** 显示包信息 *)
let show_package_info metadata =
  Printf.printf "Package: %s\n" metadata.name;
  Printf.printf "Description: %s\n" metadata.description;
  Printf.printf "Author: %s\n" metadata.author;
  Printf.printf "Repository: %s\n" metadata.repository;
  Printf.printf "Versions: %s\n" (String.concat ~sep:", " metadata.versions);
  (match get_latest_version metadata with
   | Some v -> Printf.printf "Latest: %s\n" v
   | None -> ())

(** 列出已安装的包 *)
let list_installed () =
  let deps_dir = "deps" in
  try
    let packages = Stdlib.Sys.readdir deps_dir |> Array.to_list in
    if List.is_empty packages then
      Printf.printf "No packages installed.\n"
    else begin
      Printf.printf "Installed packages:\n";
      List.iter packages ~f:(fun name ->
        let version_file = Filename.concat (Filename.concat deps_dir name) ".installed" in
        try
          let version = In_channel.read_all version_file |> String.strip in
          Printf.printf "  %s@%s\n" name version
        with Sys_error _ ->
          Printf.printf "  %s (unknown version)\n" name)
    end
  with Sys_error _ ->
    Printf.printf "No packages installed.\n"

(** 从 my-lang.toml 读取依赖并安装 *)
let install_from_config () =
  try
    let config = Package_manager.read_config () in
    if List.is_empty config.dependencies then
      Printf.printf "No dependencies to install.\n"
    else begin
      Printf.printf "Installing dependencies from my-lang.toml...\n";
      List.iter config.dependencies ~f:(fun (name, version_constraint) ->
        (* 简单处理：假设版本约束就是版本号 *)
        let version = String.strip version_constraint in
        install_package name version)
    end
  with Failure msg ->
    Printf.eprintf "Error: %s\n" msg

(** 发布包（简化版） *)
let publish_package () =
  try
    let config = Package_manager.read_config () in
    Printf.printf "Publishing %s@%s...\n" config.name config.version;
    Printf.printf "✓ Package published (simulated)\n";
    Printf.printf "  Note: In real usage, this would push to a Git repository\n"
  with Failure msg ->
    Printf.eprintf "Error: %s\n" msg
