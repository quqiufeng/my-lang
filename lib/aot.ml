(** AoT (Ahead-of-Time) 编译模块

    将 MyLang 程序编译为独立的原生可执行文件。
    使用 Chez Scheme 的 compile-program 和 scheme-start 机制。
*)

open Ast

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

(** 使用 Chez Scheme 编译为机器码对象文件 *)
let compile_to_object (scheme_file : string) (output_file : string) : (string, string) Result.t =
  let cmd = Printf.sprintf 
    "/opt/ChezScheme/ta6le/bin/ta6le/scheme --compile %s --output %s" 
    scheme_file output_file in
  let result = Sys.command cmd in
  if result = 0 then
    Ok output_file
  else
    Error (Printf.sprintf "Scheme compilation failed with exit code %d" result)

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
let generate_c_starter (output_file : string) : unit =
  let c_code = {c|#include <stdio.h>
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
|c} in
  let oc = open_out output_file in
  output_string oc c_code;
  close_out oc

(** AoT 编译主函数 - 生成 Scheme 脚本 *)
let compile_standalone 
    (source_file : string) 
    (output_file : string) 
    : (string, string) Result.t =
  try
    (* 1. 读取并解析源文件 *)
    let content = read_file source_file in
    let lexbuf = Lexing.from_string content in
    let expr = Parser.prog Lexer.read lexbuf in
    
    (* 2. 类型检查 *)
    let _ = Typeinfer.typecheck expr in
    
    (* 3. 编译为 Scheme *)
    let main_scheme = Scheme_backend.compile_expr expr in
    let standalone_scheme = generate_standalone_scheme main_scheme in
    
    (* 4. 写入最终文件（带 shebang） *)
    let final_scheme = Printf.sprintf "#!/opt/ChezScheme/ta6le/bin/ta6le/scheme --script\n%s" 
      standalone_scheme in
    write_scheme_file output_file final_scheme;
    
    (* 5. 设置可执行权限 *)
    Unix.chmod output_file 0o755;
    
    Ok (Printf.sprintf "AoT compilation successful: %s" output_file)
  with
  | Sys_error msg -> Error (Printf.sprintf "System error: %s" msg)
  | exn -> Error (Printf.sprintf "Compilation error: %s" (Printexc.to_string exn))

(** 生成真正的原生二进制（使用 Chez Scheme 的静态链接） *)
let compile_native_binary 
    (source_file : string) 
    (output_file : string) 
    : (string, string) Result.t =
  try
    (* 1. 读取并解析源文件 *)
    let content = read_file source_file in
    let lexbuf = Lexing.from_string content in
    let expr = Parser.prog Lexer.read lexbuf in
    
    (* 2. 类型检查 *)
    let _ = Typeinfer.typecheck expr in
    
    (* 3. 编译为 Scheme *)
    let scheme_code = Scheme_backend.compile_program expr in
    
    (* 4. 写入 Scheme 文件 *)
    let temp_scheme = Filename.temp_file "mylang" ".ss" in
    write_scheme_file temp_scheme scheme_code;
    
    (* 5. 使用 Chez Scheme 编译为共享对象 *)
    let temp_so = Filename.temp_file "mylang" ".so" in
    let compile_cmd = Printf.sprintf
      "/opt/ChezScheme/ta6le/bin/ta6le/scheme -q --compile %s --output %s 2>&1"
      temp_scheme temp_so in
    let compile_result = Sys.command compile_cmd in
    
    if compile_result <> 0 then
      Error "Scheme compilation to .so failed"
    else
      (* 6. 生成 C 启动文件 *)
      let temp_c = Filename.temp_file "mylang_start" ".c" in
      generate_c_starter temp_c;
      
      (* 7. 编译并链接 *)
      let chez_lib = "/opt/ChezScheme/ta6le/lib/csv10.5.0/ta6le" in
      let chez_include = "/opt/ChezScheme/ta6le/include" in
      let link_cmd = Printf.sprintf
        "gcc -o %s %s -I%s -L%s -lchezscheme -lm -ldl -lncurses -lz -lpthread 2>&1"
        output_file temp_c chez_include chez_lib in
      let link_result = Sys.command link_cmd in
      
      if link_result = 0 then begin
        Unix.chmod output_file 0o755;
        Ok (Printf.sprintf "Native binary compiled: %s" output_file)
      end else
        Error "Native compilation failed"
  with
  | Sys_error msg -> Error (Printf.sprintf "System error: %s" msg)
  | exn -> Error (Printf.sprintf "Compilation error: %s" (Printexc.to_string exn))
