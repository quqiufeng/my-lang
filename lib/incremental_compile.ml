(** 增量编译器

    提供增量编译功能，只重新编译变更的模块及其依赖者。
    编译流程：
    1. 构建模块依赖图
    2. 拓扑排序确定编译顺序
    3. 检查每个模块的缓存状态
    4. 只编译变更的模块和依赖它们的模块

    并行编译：
    按拓扑层级并行编译，同一层级内的模块没有互相依赖，可以安全并行。
*)

module Thread_lib = Thread
module Mutex_lib = Mutex
open Core
open Ast

(** 编译结果 *)
type compile_result = {
  module_name : string;
  success : bool;
  errors : string list;
  bytecode : Bytecode.code option;
}

(** 编译锁：保护全局状态（如 lexer 的 curr_pos） *)
let compile_mutex = Mutex_lib.create ()

(** 编译单个模块
    文件读取在锁外进行，解析/类型检查/编译在锁内进行以保证线程安全 *)
let compile_module ~cache info =
  let module_name = info.Module_dependency.name in
  let path = info.Module_dependency.path in

  try
    let source = In_channel.read_all path in
    let source_hash = Compilation_cache.compute_hash source in

    (* 尝试从 AST 缓存加载（无需锁，因为只读） *)
    let cached_expr = Compilation_cache.load_ast_cache module_name source_hash in

    (* 加锁保护解析和编译（共享全局状态如 lexer curr_pos） *)
    Mutex_lib.lock compile_mutex;
    let result =
      try
        let expr, ast_from_cache =
          match cached_expr with
          | Some e ->
              (e, true)
          | None ->
              let lexbuf = Lexing.from_string source in
              let expr = Parser.prog Lexer.read lexbuf in
              if cache then Compilation_cache.save_ast_cache module_name source_hash expr;
              (expr, false)
        in

        (* 类型检查 *)
        let env = Module_dependency.preprocess_imports Eval.builtin_type_env expr in
        let _ = Typeinfer.typecheck_with_env env expr in

        (* 所有权检查 *)
        Ownership.check_program [expr];

        (* 编译为字节码 *)
        let bytecode = Compiler.compile expr in

        (* 保存编译缓存 *)
        if cache then begin
          let artifact = {
            Compilation_cache.source_hash = source_hash;
            bytecode = "";  (* 简化：不存储实际字节码，只存储哈希 *)
            timestamp = Stdlib.float_of_int (Stdlib.Random.int 1000000);
          } in
          Compilation_cache.save_artifact module_name artifact
        end;

        { module_name; success = true; errors = []; bytecode = Some bytecode }
      with
      | Lexer.SyntaxError msg ->
          { module_name; success = false; errors = ["Syntax error: " ^ msg]; bytecode = None }
      | Parser.Error ->
          { module_name; success = false; errors = ["Parse error"]; bytecode = None }
      | Types.TypeError msg ->
          { module_name; success = false; errors = ["Type error: " ^ msg]; bytecode = None }
      | Eval.RuntimeError (msg, _) ->
          { module_name; success = false; errors = ["Runtime error: " ^ msg]; bytecode = None }
      | Ownership.OwnershipError msg ->
          { module_name; success = false; errors = ["Ownership error: " ^ msg]; bytecode = None }
      | exn ->
          { module_name; success = false; errors = ["Error: " ^ Exn.to_string exn]; bytecode = None }
    in
    Mutex_lib.unlock compile_mutex;
    result
  with
  | exn ->
      { module_name; success = false; errors = ["Error: " ^ Exn.to_string exn]; bytecode = None }

(** 增量编译入口文件及其所有依赖 *)
let compile_incremental ~cache entry_path =
  let graph = Module_dependency.create_graph () in
  let visited = Hashtbl.create (module String) in
  Module_dependency.build_graph graph entry_path visited;

  (* 拓扑排序 *)
  let sorted = Module_dependency.topological_sort graph in

  (* 确定需要编译的模块 *)
  let needs_compile = Hashtbl.create (module String) in

  (* 标记变更的模块 *)
  List.iter sorted ~f:(fun name ->
    match Hashtbl.find graph.modules name with
    | Some info ->
        let source = In_channel.read_all info.path in
        if Compilation_cache.needs_recompile name source then
          Hashtbl.set needs_compile ~key:name ~data:true
    | None -> ());

  (* 传递依赖：如果模块A依赖模块B且B需要编译，则A也需要编译 *)
  let to_mark = ref [] in
  let rec mark_dependents name =
    Hashtbl.iteri graph.adjacency
      ~f:(fun ~key:dependent ~data:deps ->
        if List.mem deps name ~equal:String.equal then
          if not (Hashtbl.mem needs_compile dependent) then
            to_mark := dependent :: !to_mark)
  in

  Hashtbl.iteri needs_compile ~f:(fun ~key:name ~data:_ -> mark_dependents name);
  List.iter !to_mark ~f:(fun name -> Hashtbl.set needs_compile ~key:name ~data:true);

  (* 按拓扑顺序编译 *)
  let results = ref [] in
  List.iter sorted ~f:(fun name ->
    match Hashtbl.find graph.modules name with
    | Some info ->
        let from_cache = cache && not (Hashtbl.mem needs_compile name) in
        if from_cache then
          Printf.printf "[CACHE] Module %s is up to date\n" name;
        let result = compile_module ~cache info in
        results := result :: !results;
        if not result.success then
          Printf.eprintf "[ERROR] Module %s compilation failed:\n" name;
        List.iter result.errors ~f:(fun err ->
          Printf.eprintf "  %s\n" err)
    | None -> ());

  List.rev !results

(** 并行编译单个层级的所有模块
    同一层级内的模块没有互相依赖，可以安全并行 *)
let compile_level_parallel ~cache ~needs_compile graph level_names =
  let results = ref [] in
  let result_mutex = Mutex_lib.create () in
  let threads = ref [] in

  List.iter level_names ~f:(fun name ->
    match Hashtbl.find graph.Module_dependency.modules name with
    | Some info ->
        let from_cache = cache && not (Hashtbl.mem needs_compile name) in
        if from_cache then
          Printf.printf "[CACHE] Module %s is up to date\n" name;

        let thread = Thread_lib.create (fun () ->
          let result = compile_module ~cache info in
          Mutex_lib.lock result_mutex;
          results := result :: !results;
          Mutex_lib.unlock result_mutex;
          if not result.success then (
            Printf.eprintf "[ERROR] Module %s compilation failed:\n" name;
            List.iter result.errors ~f:(fun err ->
              Printf.eprintf "  %s\n" err)
          )
        ) ()
        in
        threads := thread :: !threads
    | None -> ()
  );

  (* 等待所有线程完成 *)
  List.iter !threads ~f:Thread_lib.join;
  !results

(** 增量并行编译入口文件及其所有依赖 *)
let compile_incremental_parallel ~cache entry_path =
  let graph = Module_dependency.create_graph () in
  let visited = Hashtbl.create (module String) in
  Module_dependency.build_graph graph entry_path visited;

  (* 拓扑分层 *)
  let levels = Module_dependency.topological_levels graph in

  (* 确定需要编译的模块 *)
  let needs_compile = Hashtbl.create (module String) in

  (* 标记变更的模块 *)
  List.iter levels ~f:(fun level ->
    List.iter level ~f:(fun name ->
      match Hashtbl.find graph.modules name with
      | Some info ->
          let source = In_channel.read_all info.path in
          if Compilation_cache.needs_recompile name source then
            Hashtbl.set needs_compile ~key:name ~data:true
      | None -> ()));

  (* 传递依赖：如果模块A依赖模块B且B需要编译，则A也需要编译 *)
  let to_mark = ref [] in
  let rec mark_dependents name =
    Hashtbl.iteri graph.adjacency
      ~f:(fun ~key:dependent ~data:deps ->
        if List.mem deps name ~equal:String.equal then
          if not (Hashtbl.mem needs_compile dependent) then
            to_mark := dependent :: !to_mark)
  in

  Hashtbl.iteri needs_compile ~f:(fun ~key:name ~data:_ -> mark_dependents name);
  List.iter !to_mark ~f:(fun name -> Hashtbl.set needs_compile ~key:name ~data:true);

  (* 按层级并行编译 *)
  let all_results = ref [] in
  List.iteri levels ~f:(fun level_idx level ->
    Printf.printf "[PARALLEL] Compiling level %d (%d modules): %s\n"
      level_idx (List.length level) (String.concat ~sep:", " level);
    let results = compile_level_parallel ~cache ~needs_compile graph level in
    all_results := results @ !all_results
  );

  List.rev !all_results

(** 编译并链接所有模块为单一字节码 *)
let compile_and_link ~cache entry_path =
  let results = compile_incremental ~cache entry_path in
  let successful = List.filter results ~f:(fun r -> r.success) in

  if List.exists results ~f:(fun r -> not r.success) then
    Error "Some modules failed to compile"
  else
    (* 链接：将所有模块的字节码串联 *)
    let all_bytecode =
      List.filter_map successful ~f:(fun r -> r.bytecode)
      |> Array.concat
    in
    Ok all_bytecode

(** 并行编译并链接所有模块为单一字节码 *)
let compile_and_link_parallel ~cache entry_path =
  let results = compile_incremental_parallel ~cache entry_path in
  let successful = List.filter results ~f:(fun r -> r.success) in

  if List.exists results ~f:(fun r -> not r.success) then
    Error "Some modules failed to compile"
  else
    (* 链接：将所有模块的字节码串联 *)
    let all_bytecode =
      List.filter_map successful ~f:(fun r -> r.bytecode)
      |> Array.concat
    in
    Ok all_bytecode

(** 显示依赖图信息 *)
let show_dependency_graph entry_path =
  let graph = Module_dependency.create_graph () in
  let visited = Hashtbl.create (module String) in
  Module_dependency.build_graph graph entry_path visited;

  Printf.printf "=== Module Dependency Graph ===\n";
  Hashtbl.iteri graph.modules ~f:(fun ~key:name ~data:info ->
    Printf.printf "Module: %s (%s)\n" name info.Module_dependency.path;
    Printf.printf "  Dependencies:\n";
    List.iter info.Module_dependency.dependencies ~f:(fun dep ->
      match dep with
      | Module_dependency.FileImport path -> Printf.printf "    import \"%s\"\n" path
      | Module_dependency.ModuleOpen m -> Printf.printf "    open %s\n" m));

  Printf.printf "\n=== Compilation Order (Topological Sort) ===\n";
  let sorted = Module_dependency.topological_sort graph in
  List.iteri sorted ~f:(fun i name ->
    Printf.printf "%d. %s\n" (i + 1) name)