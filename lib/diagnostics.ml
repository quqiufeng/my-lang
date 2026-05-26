(** 诊断管理器

    提供编译全过程中的错误聚合、统计和报告。
    支持多个编译阶段（lex/parse/typecheck/codegen）的错误收集。
*)

open Core

type phase =
  | Lexing
  | Parsing
  | TypeChecking
  | OwnershipCheck
  | CodeGen
  | Runtime
  | Linking
[@@deriving sexp]

type t = {
  mutable diagnostics : Error_context.diagnostic list;
  mutable has_errors : bool;
  mutable phase_counts : (phase * int) list;
}

let create () = {
  diagnostics = [];
  has_errors = false;
  phase_counts = [];
}

let add_diagnostic t ?(phase=Runtime) d =
  t.diagnostics <- d :: t.diagnostics;
  (match d.Error_context.severity with
   | Error_context.Error -> t.has_errors <- true
   | _ -> ());
  let counts = List.Assoc.find t.phase_counts phase ~equal:Poly.equal in
  let new_count = Option.value counts ~default:0 + 1 in
  t.phase_counts <- List.Assoc.add t.phase_counts ~equal:Poly.equal phase new_count

let add_error t ?(phase=Runtime) ?(line=0) ?(col=0) ?source_line ?(highlight_len=1) ?help message =
  let d = {
    Error_context.severity = Error_context.Error;
    message;
    file = "";
    line;
    col;
    source_line;
    highlight_len;
    help;
  } in
  add_diagnostic t ~phase d

let add_warning t ?(phase=Runtime) ?(line=0) ?(col=0) ?source_line ?(highlight_len=1) ?help message =
  let d = {
    Error_context.severity = Error_context.Warning;
    message;
    file = "";
    line;
    col;
    source_line;
    highlight_len;
    help;
  } in
  add_diagnostic t ~phase d

let from_lexbuf t ~phase ~severity ~message ?help lexbuf =
  let d = Error_context.from_lexbuf ~severity ~message ?help lexbuf in
  add_diagnostic t ~phase d

let from_exception t ?(phase=Runtime) ?(file="") exn lexbuf_opt =
  let d = Error_context.from_exception ~file exn lexbuf_opt in
  add_diagnostic t ~phase d

let has_errors t = t.has_errors

let error_count t =
  List.count t.diagnostics ~f:(fun d ->
    Poly.equal d.Error_context.severity Error_context.Error)

let warning_count t =
  List.count t.diagnostics ~f:(fun d ->
    Poly.equal d.Error_context.severity Error_context.Warning)

let note_count t =
  List.count t.diagnostics ~f:(fun d ->
    Poly.equal d.Error_context.severity Error_context.Note)

let sort_by_location t =
  { t with
    diagnostics = List.sort t.diagnostics ~compare:(fun a b ->
      let cmp = String.compare a.Error_context.file b.Error_context.file in
      if cmp <> 0 then cmp
      else
        let cmp = Int.compare a.Error_context.line b.Error_context.line in
        if cmp <> 0 then cmp
        else Int.compare a.Error_context.col b.Error_context.col
    )
  }

let limit t n =
  { t with
    diagnostics = List.take t.diagnostics n;
  }

let format_all ?(max_errors=10) t =
  let sorted = sort_by_location t in
  let to_show = if List.length sorted.diagnostics > max_errors
    then List.take sorted.diagnostics max_errors
    else sorted.diagnostics
  in
  let ctx = Error_context.create () in
  List.iter to_show ~f:(fun d ->
    Error_context.add_diagnostic ctx
      ~severity:d.Error_context.severity
      ~message:d.Error_context.message
      ~file:d.Error_context.file
      ~line:d.Error_context.line
      ~col:d.Error_context.col
      ?source_line:d.Error_context.source_line
      ~highlight_len:d.Error_context.highlight_len
      ?help:d.Error_context.help ());
  let result = Error_context.format_all ctx in
  let remaining = List.length sorted.diagnostics - max_errors in
  if remaining > 0 then
    result ^ Printf.sprintf "\n... 以及 %d 个额外的诊断信息 ...\n" remaining
  else
    result

let format_summary t =
  let errors = error_count t in
  let warnings = warning_count t in
  let notes = note_count t in
  let parts = [] in
  let parts = if errors > 0 then Printf.sprintf "%d 个错误" errors :: parts else parts in
  let parts = if warnings > 0 then Printf.sprintf "%d 个警告" warnings :: parts else parts in
  let parts = if notes > 0 then Printf.sprintf "%d 个提示" notes :: parts else parts in
  match parts with
  | [] -> "编译成功，没有诊断信息"
  | _ -> "诊断摘要: " ^ String.concat ~sep:"，" parts
