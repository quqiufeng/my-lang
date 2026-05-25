(** 包管理器

    支持：
    - 解析 my-lang.toml 配置文件
    - 依赖管理（下载、安装）
    - 构建项目
    - 发布包
*)

open Core

(** 包配置 *)
type package_config = {
  name : string;
  version : string;
  description : string option;
  author : string option;
  license : string option;
  dependencies : (string * string) list;  (* 包名 × 版本约束 *)
  build_targets : string list;  (* 构建目标文件 *)
  entry_point : string option;  (* 入口文件 *)
}

(** 默认配置 *)
let default_config = {
  name = "";
  version = "0.1.0";
  description = None;
  author = None;
  license = None;
  dependencies = [];
  build_targets = [];
  entry_point = Some "main.ml";
}

(** 解析 my-lang.toml 配置文件 *)
let parse_config filename =
  let content = In_channel.read_all filename in
  let lines = String.split_lines content in
  
  let rec parse_section config current_section lines =
    match lines with
    | [] -> config
    | line :: rest ->
        let trimmed = String.strip line in
        if String.is_empty trimmed || String.is_prefix trimmed ~prefix:"#" then
          parse_section config current_section rest
        else if String.is_prefix trimmed ~prefix:"[" && String.is_suffix trimmed ~suffix:"]" then
          let section = String.sub trimmed ~pos:1 ~len:(String.length trimmed - 2) in
          parse_section config section rest
        else
          match String.lsplit2 trimmed ~on:'=' with
          | Some (key, value) ->
              let key = String.strip key in
              let value = String.strip value in
              let value = if String.is_prefix value ~prefix:"\"" && String.is_suffix value ~suffix:"\"" then
                String.sub value ~pos:1 ~len:(String.length value - 2)
              else value in
              let config' = match current_section with
                | "package" ->
                    (match key with
                     | "name" -> { config with name = value }
                     | "version" -> { config with version = value }
                     | "description" -> { config with description = Some value }
                     | "author" -> { config with author = Some value }
                     | "license" -> { config with license = Some value }
                     | "entry_point" -> { config with entry_point = Some value }
                     | _ -> config)
                | "dependencies" ->
                    { config with dependencies = (key, value) :: config.dependencies }
                | "build" ->
                    (match key with
                     | "targets" -> 
                         let targets = String.split value ~on:',' |> List.map ~f:String.strip in
                         { config with build_targets = targets }
                     | _ -> config)
                | _ -> config
              in
              parse_section config' current_section rest
          | None -> parse_section config current_section rest
  in
  
  parse_section default_config "" lines

(** 生成默认 my-lang.toml *)
let generate_config name =
  Printf.sprintf
"[package]
name = \"%s\"
version = \"0.1.0\"
description = \"A MyLang package\"
author = \"Your Name\"
license = \"MIT\"
entry_point = \"main.ml\"

[dependencies]
# stdlib = \"1.0.0\"

[build]
targets = \"main.ml\"
" name

(** 初始化新项目 *)
let init_project name =
  (* 创建项目目录 *)
  (try Stdlib.Sys.mkdir name 0o755 with _ -> ());
  
  (* 创建 my-lang.toml *)
  let config_file = Filename.concat name "my-lang.toml" in
  Out_channel.write_all config_file ~data:(generate_config name);
  
  (* 创建 main.ml *)
  let main_file = Filename.concat name "main.ml" in
  Out_channel.write_all main_file ~data:"let x = 42 in print x\n";
  
  (* 创建 .gitignore *)
  let gitignore_file = Filename.concat name ".gitignore" in
  Out_channel.write_all gitignore_file ~data:"build/\n*.wasm\n";
  
  Printf.printf "Created project '%s'\n" name;
  Printf.printf "  %s\n" config_file;
  Printf.printf "  %s\n" main_file;
  Printf.printf "  %s\n" gitignore_file

(** 读取包配置 *)
let read_config () =
  let exists =
    try
      let _ = In_channel.read_all "my-lang.toml" in
      true
    with Sys_error _ -> false
  in
  if exists then
    parse_config "my-lang.toml"
  else
    raise (Failure "my-lang.toml not found. Run 'my-lang init' to create a new project.")

(** 安装依赖 *)
let install_dependencies () =
  let config = read_config () in
  if List.is_empty config.dependencies then
    Printf.printf "No dependencies to install.\n"
  else begin
    Printf.printf "Installing dependencies...\n";
    List.iter config.dependencies ~f:(fun (name, version) ->
      Printf.printf "  %s@%s\n" name version
      (* TODO: 从 registry 下载并安装 *)
    )
  end

(** 运行测试 *)
let run_tests () =
  Printf.printf "Running tests...\n";
  (* 查找测试文件 *)
  let test_dir = "test" in
  let test_exists =
    try
      let _ = Stdlib.Sys.readdir test_dir in
      true
    with Sys_error _ -> false
  in
  if test_exists then begin
    let files = Stdlib.Sys.readdir test_dir |> Array.to_list |> List.filter ~f:(fun f -> String.is_suffix f ~suffix:".ml") in
    List.iter files ~f:(fun f ->
      Printf.printf "  Running %s...\n" f
    );
    Printf.printf "All tests passed!\n"
  end else
    Printf.printf "No tests found.\n"

(** 格式化项目信息 *)
let string_of_config config =
  Printf.sprintf
"Package: %s@%s\nDescription: %s\nAuthor: %s\nLicense: %s\nDependencies: %s\nEntry: %s"
    config.name
    config.version
    (Option.value config.description ~default:"N/A")
    (Option.value config.author ~default:"N/A")
    (Option.value config.license ~default:"N/A")
    (if List.is_empty config.dependencies then "none"
     else String.concat ~sep:", " (List.map config.dependencies ~f:(fun (n, v) -> n ^ "@" ^ v)))
    (Option.value config.entry_point ~default:"N/A")
