(** AoT (Ahead-of-Time) 编译模块

    将 MyLang 程序编译为独立的原生可执行文件。
    使用 Chez Scheme 的 compile-program 和 scheme-start 机制。

    支持两种模式：
    1. shebang 脚本：依赖本地 Chez Scheme 安装
    2. 原生二进制：通过静态链接 Chez Scheme 运行时（需要完整的 Chez Scheme 开发包）

    配置方式（按优先级）：
    - MYLANG_CHEZ_SCHEME 环境变量
    - CHEZ_SCHEME_HOME 环境变量
    - 默认搜索路径：/opt/ChezScheme
*)

open Ast

(** Chez Scheme 安装路径检测 *)
let detect_chez_scheme () : string option =
  let env_paths = [
    (try Some (Unix.getenv "MYLANG_CHEZ_SCHEME") with Not_found -> None);
    (try Some (Unix.getenv "CHEZ_SCHEME_HOME") with Not_found -> None);
  ] in
  match List.find_opt (function Some _ -> true | None -> false) env_paths with
  | Some (Some path) -> Some path
  | _ ->
      (* 检查默认路径 *)
      let default_paths = ["/opt/ChezScheme"; "/usr/local/lib/chezscheme"; "/usr/lib/chezscheme"] in
      List.find_opt Sys.file_exists default_paths

(** Chez Scheme 可执行文件路径 *)
let chez_executable () : string option =
  match detect_chez_scheme () with
  | Some home ->
      let candidates = [
        Filename.concat home "bin/ta6le/scheme";
        Filename.concat home "bin/scheme";
        Filename.concat home "ta6le/bin/scheme";
      ] in
      List.find_opt Sys.file_exists candidates
  | None -> None

(** 读取文件内容 *)
let read_file (filename : string) : string =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

(** 编译 MyLang 表达式为 Scheme 源码 *)
let compile_to_scheme (expr : Ast.expr) : string =
  Scheme_backend.compile_program expr

(** 写入 Scheme 文件 *)
let write_scheme_file (filename : string) (scheme_code : string) : unit =
  let oc = open_out filename in
  output_string oc scheme_code;
  close_out oc

(** 生成独立可执行文件的 Scheme 启动代码 *)
let generate_standalone_scheme (main_scheme : string) : string =
  Printf.sprintf
{|;; MyLang AoT 编译生成的独立可执行文件
;; 自动生成，请勿手动修改

(import (chezscheme))

;; 主程序
(define (mylang-main)
  (display %s)
  (newline))

;; 入口点
(mylang-main)
|} main_scheme

(** 生成 C 启动代码，用于链接 Scheme 编译的对象文件 *)
let generate_c_starter () : string =
  {|
#include <stdio.h>
#include <stdlib.h>

/* Chez Scheme 启动函数 */
extern void scheme_start(int argc, char **argv);
extern void Sscheme_init(void *ptr);

int main(int argc, char **argv) {
  /* 初始化 Chez Scheme 运行时 */
  Sscheme_init(NULL);

  /* 启动 Scheme 程序 */
  scheme_start(argc, argv);

  return 0;
}
|}

(** 内部：编译流程核心逻辑 *)
let compile_core (source_file : string) (f : Ast.expr -> (string, string) Result.t) : (string, string) Result.t =
  try
    let content = read_file source_file in
    let lexbuf = Lexing.from_string content in
    let expr = Parser.prog Lexer.read lexbuf in
    let _ = Typeinfer.typecheck expr in
    f expr
  with
  | Sys_error msg -> Error (Printf.sprintf "System error: %s" msg)
  | exn -> Error (Printf.sprintf "Compilation error: %s" (Printexc.to_string exn))

(** AoT 编译主函数 - 生成 Scheme 脚本（shebang 方式） *)
let compile_standalone
    (source_file : string)
    (output_file : string)
    : (string, string) Result.t =
  compile_core source_file (fun expr ->
    let main_scheme = Scheme_backend.compile_expr expr in
    let standalone_scheme = generate_standalone_scheme main_scheme in
    let chez_path = match chez_executable () with
      | Some path -> path
      | None -> "scheme"  (* 回退到 PATH 搜索 *)
    in
    let final_scheme = Printf.sprintf "#!%s --script\n%s" chez_path standalone_scheme in
    write_scheme_file output_file final_scheme;
    Unix.chmod output_file 0o755;
    Ok (Printf.sprintf "AoT compilation successful: %s (shebang script)" output_file)
  )

(** 生成真正的原生二进制（使用 Chez Scheme 的静态链接） *)
let compile_native_binary
    (source_file : string)
    (output_file : string)
    : (string, string) Result.t =
  match detect_chez_scheme () with
  | None ->
      Error "Chez Scheme not found. Set MYLANG_CHEZ_SCHEME or CHEZ_SCHEME_HOME environment variable."
  | Some chez_home ->
      compile_core source_file (fun expr ->
        let scheme_code = Scheme_backend.compile_program expr in

        (* 写入临时 Scheme 文件 *)
        let temp_scheme = Filename.temp_file "mylang" ".ss" in
        write_scheme_file temp_scheme scheme_code;

        (* 编译为共享对象 *)
        let temp_so = Filename.temp_file "mylang" ".so" in
        let chez_bin = Filename.concat chez_home "bin/ta6le/scheme" in
        let compile_cmd = Printf.sprintf
          "%s -q --compile %s --output %s 2>&1"
          chez_bin temp_scheme temp_so in
        let compile_result = Sys.command compile_cmd in

        if compile_result <> 0 then
          Error "Scheme compilation to .so failed"
        else
          (* 生成 C 启动文件 *)
          let temp_c = Filename.temp_file "mylang_start" ".c" in
          let c_code = generate_c_starter () in
          let oc = open_out temp_c in
          output_string oc c_code;
          close_out oc;

          (* 编译并链接 *)
          let chez_lib = Filename.concat chez_home "lib/csv10.5.0/ta6le" in
          let chez_include = Filename.concat chez_home "include" in
          let link_cmd = Printf.sprintf
            "gcc -o %s %s -I%s -L%s -lchezscheme -lm -ldl -lncurses -lz -lpthread 2>&1"
            output_file temp_c chez_include chez_lib in
          let link_result = Sys.command link_cmd in

          if link_result = 0 then begin
            Unix.chmod output_file 0o755;
            Ok (Printf.sprintf "Native binary compiled: %s" output_file)
          end else
            Error "Native compilation failed"
      )
