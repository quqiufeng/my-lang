(** 模块依赖分析

    分析源文件中的依赖关系，构建模块依赖图。
    支持的依赖形式：
    - import "filename.ml"
    - open Module
    - module M = ... (内部模块，不产生文件依赖)
*)

open Core
open Ast

(** 依赖类型 *)
type dependency =
  | FileImport of string  (** 文件导入，如 import "foo.ml" *)
  | ModuleOpen of string  (** 模块打开，如 open Foo *)

(** 模块信息 *)
type module_info = {
  name : string;  (** 模块名，通常是文件名（不含扩展名） *)
  path : string;  (** 文件路径 *)
  dependencies : dependency list;  (** 依赖列表 *)
  mutable hash : string option;  (** 内容哈希，用于缓存 *)
}

(** 提取表达式中的依赖 *)
let rec extract_deps expr acc =
  match expr with
  | EApp (EVar "import", EString filename) ->
      FileImport filename :: acc
  | EOpen name ->
      ModuleOpen name :: acc
  | ESeq (e1, e2) | ELet (_, e1, e2) | ELetRec (_, e1, e2) ->
      let acc' = extract_deps e1 acc in
      extract_deps e2 acc'
  | EIf (cond, t, f) ->
      let acc' = extract_deps cond acc in
      let acc'' = extract_deps t acc' in
      extract_deps f acc''
  | EMatch (e, cases) ->
      let acc' = extract_deps e acc in
      List.fold_left cases ~init:acc' ~f:(fun acc (_, body) -> extract_deps body acc)
  | EFun (_, body) ->
      extract_deps body acc
  | EApp (e1, e2) ->
      let acc' = extract_deps e1 acc in
      extract_deps e2 acc'
  | ETuple es | EList es ->
      List.fold_left es ~init:acc ~f:(fun acc e -> extract_deps e acc)
  | ERecord fields ->
      List.fold_left fields ~init:acc ~f:(fun acc (_, e) -> extract_deps e acc)
  | ERecordUpdate (e, fields) ->
      let acc' = extract_deps e acc in
      List.fold_left fields ~init:acc' ~f:(fun acc (_, e) -> extract_deps e acc)
  | EArray es ->
      List.fold_left es ~init:acc ~f:(fun acc e -> extract_deps e acc)
  | EArrayGet (e1, e2) ->
      let acc' = extract_deps e1 acc in
      extract_deps e2 acc'
  | ERef e | EDeref e | ENot e ->
      extract_deps e acc
  | EAssign (e1, e2) ->
      let acc' = extract_deps e1 acc in
      extract_deps e2 acc'
  | EWhile (cond, body) ->
      let acc' = extract_deps cond acc in
      extract_deps body acc'
  | ETry (e, cases) ->
      let acc' = extract_deps e acc in
      List.fold_left cases ~init:acc' ~f:(fun acc (_, body) -> extract_deps body acc)
  | EModule (_, body) ->
      extract_deps body acc
  | EHandle (e, handlers) ->
      let acc' = extract_deps e acc in
      List.fold_left handlers ~init:acc' ~f:(fun acc (_, _, _, body) -> extract_deps body acc)
  | EPerform (_, e) ->
      extract_deps e acc
  | ESpawn e | ESend (_, e) | EAnnot (e, _) ->
      extract_deps e acc
  | _ -> acc

(** 分析文件，获取模块信息 *)
let analyze_file path =
  let content =
    try Core.In_channel.read_all path
    with Sys_error msg -> failwith ("Cannot read file: " ^ msg)
  in
  let lexbuf = Lexing.from_string content in
  let expr = Parser.prog Lexer.read lexbuf in
  let deps = extract_deps expr [] in
  let name =
    let basename = Filename.basename path in
    try Filename.chop_extension basename
    with Invalid_argument _ -> basename
  in
  { name; path; dependencies = deps; hash = None }

(** 依赖图 *)
type dependency_graph = {
  modules : (string, module_info) Hashtbl.t;  (** 模块名 -> 模块信息 *)
  adjacency : (string, string list) Hashtbl.t;  (** 模块名 -> 依赖的模块名列表 *)
}

let create_graph () = {
  modules = Hashtbl.create (module String);
  adjacency = Hashtbl.create (module String);
}

(** 添加模块到依赖图 *)
let add_module graph info =
  Hashtbl.set graph.modules ~key:info.name ~data:info;
  let dep_names =
    List.filter_map info.dependencies ~f:(function
      | FileImport path ->
          let basename = Filename.basename path in
          Some (try Filename.chop_extension basename with Invalid_argument _ -> basename)
      | ModuleOpen name -> Some name)
  in
  Hashtbl.set graph.adjacency ~key:info.name ~data:dep_names

(** 构建文件的依赖图（递归分析所有导入的文件） *)
let rec build_graph graph path visited =
  if Hashtbl.mem visited path then ()
  else begin
    Hashtbl.set visited ~key:path ~data:true;
    try
      let info = analyze_file path in
      add_module graph info;
      (* 递归分析导入的文件 *)
      List.iter info.dependencies ~f:(function
        | FileImport import_path ->
            let resolved_path =
              if Filename.is_relative import_path then
                Filename.concat (Filename.dirname path) import_path
              else import_path
            in
            if Stdlib.Sys.file_exists resolved_path then
              build_graph graph resolved_path visited
        | ModuleOpen _ -> ())
    with exn ->
      Printf.eprintf "Warning: failed to analyze %s: %s\n" path (Exn.to_string exn)
  end

(** 拓扑排序（Kahn算法） *)
let topological_sort graph =
  let in_degree = Hashtbl.create (module String) in
  let queue = Queue.create () in
  let result = ref [] in

  (* 计算入度：模块的入度 = 它依赖的其他模块数量 *)
  Hashtbl.iteri graph.modules ~f:(fun ~key:name ~data:_ ->
    let deps = match Hashtbl.find graph.adjacency name with Some d -> d | None -> [] in
    Hashtbl.set in_degree ~key:name ~data:(List.length deps));

  (* 构建反向邻接表：dep -> [modules that depend on dep] *)
  let reverse_adj = Hashtbl.create (module String) in
  Hashtbl.iteri graph.adjacency ~f:(fun ~key:name ~data:deps ->
    List.iter deps ~f:(fun dep ->
      let dependents = match Hashtbl.find reverse_adj dep with Some d -> d | None -> [] in
      Hashtbl.set reverse_adj ~key:dep ~data:(name :: dependents)));

  (* 找入度为0的节点（没有依赖的模块） *)
  Hashtbl.iteri in_degree ~f:(fun ~key:name ~data:degree -> if degree = 0 then Queue.enqueue queue name);

  (* BFS *)
  while not (Queue.is_empty queue) do
    let name = Queue.dequeue_exn queue in
    result := name :: !result;
    let dependents = match Hashtbl.find reverse_adj name with Some d -> d | None -> [] in
    List.iter dependents ~f:(fun dependent ->
      let degree = match Hashtbl.find in_degree dependent with Some d -> d - 1 | None -> -1 in
      Hashtbl.set in_degree ~key:dependent ~data:degree;
      if degree = 0 then Queue.enqueue queue dependent)
  done;

  (* 检查是否有环 *)
  let total_modules = Hashtbl.length graph.modules in
  if List.length !result <> total_modules then
    failwith "Circular dependency detected";

  List.rev !result

(** 预处理 import，收集导入文件中的类型绑定 *)
let rec preprocess_imports env expr =
  match expr with
  | EApp (EVar "import", EString filename) ->
      let content =
        try Core.In_channel.read_all filename
        with Sys_error msg -> raise (Types.TypeError ("Cannot import file: " ^ msg))
      in
      let lexbuf = Lexing.from_string content in
      let imported_expr = Parser.prog Lexer.read lexbuf in
      Typeinfer.extract_bindings env imported_expr
  | ESeq (e1, e2) | ELet (_, e1, e2) | ELetRec (_, e1, e2) ->
      let env' = preprocess_imports env e1 in
      preprocess_imports env' e2
  | EIf (cond, t, f) ->
      let env' = preprocess_imports env cond in
      let env'' = preprocess_imports env' t in
      preprocess_imports env'' f
  | EMatch (e, cases) ->
      let env' = preprocess_imports env e in
      List.fold_left cases ~init:env' ~f:(fun env (_, body) -> preprocess_imports env body)
  | _ -> env