(** 符号表 - 用于 LSP 跳转到定义

    记录每个标识符的定义位置和类型信息。
*)

open Core
open Ast

(** 符号定义 *)
type symbol_def = {
  name : string;
  pos : pos;
  symbol_type : string;  (* "variable", "function", "type", "module" *)
  doc : string option;
}

(** 符号表 *)
type symbol_table = {
  mutable defs : (string, symbol_def) Hashtbl.t;
  mutable refs : (string, pos list) Hashtbl.t;  (* 引用位置 *)
}

let create () = {
  defs = Hashtbl.create (module String);
  refs = Hashtbl.create (module String);
}

(** 添加定义 *)
let add_def table name pos ~symbol_type ?doc () =
  let def = { name; pos; symbol_type; doc } in
  Hashtbl.set table.defs ~key:name ~data:def

(** 添加引用 *)
let add_ref table name pos =
  let refs = match Hashtbl.find table.refs name with
    | Some existing -> pos :: existing
    | None -> [pos]
  in
  Hashtbl.set table.refs ~key:name ~data:refs

(** 查找定义 *)
let find_def table name =
  Hashtbl.find table.defs name

(** 查找引用 *)
let find_refs table name =
  match Hashtbl.find table.refs name with
  | Some refs -> refs
  | None -> []

(** 提取 AST 中的所有符号 *)
let extract_symbols expr =
  let table = create () in
  
  let rec walk env pos expr =
    match expr with
    | EVar name ->
        add_ref table name pos
    | ELet (name, value, body) ->
        add_def table name pos ~symbol_type:"variable" ();
        walk env pos value;
        walk env pos body
    | ELetRec (name, value, body) ->
        add_def table name pos ~symbol_type:"function" ();
        walk env pos value;
        walk env pos body
    | EFun (param, body) ->
        add_def table param pos ~symbol_type:"variable" ();
        walk env pos body
    | EApp (func, arg) ->
        walk env pos func;
        walk env pos arg
    | EIf (cond, t, f) ->
        walk env pos cond;
        walk env pos t;
        walk env pos f
    | EMatch (e, cases) ->
        walk env pos e;
        List.iter cases ~f:(fun (pat, body) ->
          walk_pattern env pos pat;
          walk env pos body)
    | ESeq (e1, e2) ->
        walk env pos e1;
        walk env pos e2
    | ETuple es | EList es | EArray es ->
        List.iter es ~f:(walk env pos)
    | ERecord fields ->
        List.iter fields ~f:(fun (_, e) -> walk env pos e)
    | ERecordGet (e, _) | EDeref e | ERef e | ERaise e ->
        walk env pos e
    | EAssign (e1, e2) | EArrayGet (e1, e2) | EIndex (e1, e2) | ECons (e1, e2) | ECat (e1, e2)
    | EAdd (e1, e2) | ESub (e1, e2) | EMul (e1, e2) | EDiv (e1, e2)
    | EEq (e1, e2) | ENeq (e1, e2) | ELt (e1, e2) | ELe (e1, e2) | EGt (e1, e2) | EGe (e1, e2)
    | EAnd (e1, e2) | EOr (e1, e2) ->
        walk env pos e1;
        walk env pos e2
    | ENot e ->
        walk env pos e
    | EWhile (cond, body) ->
        walk env pos cond;
        walk env pos body
    | ETypeDef (name, _, ctors) ->
        add_def table name pos ~symbol_type:"type" ();
        List.iter ctors ~f:(fun (ctor_name, _, _) ->
          add_def table ctor_name pos ~symbol_type:"constructor" ())
    | ECtor (_, Some e) | EAnnot (e, _) ->
        walk env pos e
    | EModule (name, body) ->
        add_def table name pos ~symbol_type:"module" ();
        walk env pos body
    | EOpen name ->
        add_ref table name pos
    | EDot (e, field) ->
        walk env pos e;
        add_ref table field pos
    | ETry (e, cases) ->
        walk env pos e;
        List.iter cases ~f:(fun (pat, body) ->
          walk_pattern env pos pat;
          walk env pos body)
    | ERecordUpdate (e, fields) ->
        walk env pos e;
        List.iter fields ~f:(fun (_, e) -> walk env pos e)
    | ERange (e1, e2) ->
        walk env pos e1;
        walk env pos e2
    | ESlice (e1, e2_opt, _) ->
        walk env pos e1;
        Option.iter e2_opt ~f:(walk env pos)
    | _ -> ()
  
  and walk_pattern env pos = function
    | PVar name | PCtor (name, _) ->
        add_def table name pos ~symbol_type:"variable" ()
    | PTuple ps | PList ps ->
        List.iter ps ~f:(walk_pattern env pos)
    | PCons (p1, p2) ->
        walk_pattern env pos p1;
        walk_pattern env pos p2
    | PRecord fields ->
        List.iter fields ~f:(fun (_, p) -> walk_pattern env pos p)
    | _ -> ()
  in
  
  walk [] { line = 1; col = 1 } expr;
  table

(** 获取文档中某位置的标识符 *)
let get_identifier_at_pos content line col =
  let lines = String.split_lines content in
  if line < 0 || line >= List.length lines then None
  else
    let current_line = List.nth_exn lines line in
    if col < 0 || col >= String.length current_line then None
    else
      (* 找到光标所在的单词 *)
      let rec find_word_start c =
        if c <= 0 then 0
        else if Char.is_alphanum current_line.[c - 1] || Char.equal current_line.[c - 1] '_' then
          find_word_start (c - 1)
        else c
      in
      let rec find_word_end c =
        if c >= String.length current_line then c
        else if Char.is_alphanum current_line.[c] || Char.equal current_line.[c] '_' then
          find_word_end (c + 1)
        else c
      in
      let start = find_word_start col in
      let finish = find_word_end col in
      if finish > start then
        Some (String.sub current_line ~pos:start ~len:(finish - start))
      else None

(** 将行号列号转为 LSP 位置格式 *)
let pos_to_lsp p =
  `Assoc [
    ("line", `Int (p.line - 1));
    ("character", `Int (p.col - 1))
  ]

(** 查找定义并返回 LSP Location *)
let find_definition table content line col =
  match get_identifier_at_pos content line col with
  | None -> None
  | Some ident ->
      (match find_def table ident with
       | Some def ->
           Some (`Assoc [
             ("uri", `String "file:///");  (* 简化处理 *)
             ("range", `Assoc [
               ("start", pos_to_lsp def.pos);
               ("end", pos_to_lsp def.pos)
             ])
           ])
       | None -> None)
