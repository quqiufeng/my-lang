(** 代码格式化器

    将 AST 格式化为标准化的代码格式。
    使用 2 空格缩进，在适当位置换行。
*)

open Ast

type fmt_ctx = {
  indent : int;
  width : int;  (* 行宽限制 *)
}

let default_ctx = { indent = 0; width = 80 }

let spaces n = String.make n ' '

let indent ctx = { ctx with indent = ctx.indent + 2 }
let dedent ctx = { ctx with indent = max 0 (ctx.indent - 2) }

let fmt_line ctx s = spaces ctx.indent ^ s

let rec needs_parens = function
  | EFun _ | EIf _ | ELet _ | ELetRec _ | EMatch _ | ESeq _ | EWhile _ 
  | ETry _ | EHandle _ | EAssign _ | EOpen _ | EModule _ | EModuleType _ 
  | ETraitDef _ | ETraitImpl _ | EEffectDef _ | ETypeDef _ -> true
  | EAnd _ | EOr _ | EEq _ | ENeq _ | ELt _ | ELe _ | EGt _ | EGe _ 
  | EAdd _ | ESub _ | EMul _ | EDiv _ | ECat _ | ECons _ -> true
  | EApp (EVar _, _) -> false
  | EApp _ -> true
  | _ -> false

let rec format_expr ctx expr =
  match expr with
  | EInt n -> string_of_int n
  | EBool true -> "true"
  | EBool false -> "false"
  | EChar c -> Printf.sprintf "'%c'" c
  | EString s -> Printf.sprintf "\"%s\"" (String.escaped s)
  | EVar x -> x
  | EList es -> "[" ^ String.concat "; " (List.map (format_expr ctx) es) ^ "]"
  | ETuple [] -> "()"
  | ETuple [e] -> format_expr ctx e
  | ETuple es -> "(" ^ String.concat ", " (List.map (format_expr ctx) es) ^ ")"
  | EArray es -> "[|" ^ String.concat "; " (List.map (format_expr ctx) es) ^ "|]"

  | EAdd (a, b) -> format_binary ctx "+" a b
  | ESub (a, b) -> format_binary ctx "-" a b
  | EMul (a, b) -> format_binary ctx "*" a b
  | EDiv (a, b) -> format_binary ctx "/" a b
  | EEq (a, b) -> format_binary ctx "=" a b
  | ENeq (a, b) -> format_binary ctx "<>" a b
  | ELt (a, b) -> format_binary ctx "<" a b
  | ELe (a, b) -> format_binary ctx "<=" a b
  | EGt (a, b) -> format_binary ctx ">" a b
  | EGe (a, b) -> format_binary ctx ">=" a b
  | EAnd (a, b) -> format_binary ctx "&&" a b
  | EOr (a, b) -> format_binary ctx "||" a b
  | ECat (a, b) -> format_binary ctx "^" a b
  | ECons (a, b) -> format_binary ctx "::" a b

  | ENot e -> "not " ^ format_expr_atomic ctx e
  | EDeref e -> "!" ^ format_expr_atomic ctx e
  | ERef e -> "ref " ^ format_expr_atomic ctx e
  | ERaise e -> "raise " ^ format_expr_atomic ctx e

  | EAnnot (e, ty) -> format_expr ctx e ^ " : " ^ ty

  | EFun (param, body) ->
      let body_ctx = indent ctx in
      "fun " ^ param ^ " ->\n" ^ fmt_line body_ctx (format_expr body_ctx body)

  | EIf (cond, t, f) ->
      let cond_str = format_expr ctx cond in
      let t_str = format_expr ctx t in
      let f_str = format_expr ctx f in
      if String.length cond_str + String.length t_str + String.length f_str + 20 < ctx.width then
        "if " ^ cond_str ^ " then " ^ t_str ^ " else " ^ f_str
      else
        let body_ctx = indent ctx in
        "if " ^ cond_str ^ " then\n" ^ fmt_line body_ctx t_str ^ "\n" ^ 
        fmt_line ctx "else\n" ^ fmt_line body_ctx f_str

  | ELet (x, e1, e2) ->
      let e1_str = format_expr ctx e1 in
      let e2_str = format_expr ctx e2 in
      if String.length e1_str + String.length e2_str + String.length x + 15 < ctx.width then
        "let " ^ x ^ " = " ^ e1_str ^ " in " ^ e2_str
      else
        let body_ctx = indent ctx in
        "let " ^ x ^ " = " ^ e1_str ^ " in\n" ^ fmt_line body_ctx e2_str

  | ELetRec (x, e1, e2) ->
      let e1_str = format_expr ctx e1 in
      let e2_str = format_expr ctx e2 in
      if String.length e1_str + String.length e2_str + String.length x + 20 < ctx.width then
        "let rec " ^ x ^ " = " ^ e1_str ^ " in " ^ e2_str
      else
        let body_ctx = indent ctx in
        "let rec " ^ x ^ " = " ^ e1_str ^ " in\n" ^ fmt_line body_ctx e2_str

  | EApp (f, arg) ->
      format_expr ctx f ^ " " ^ format_expr_atomic ctx arg

  | EMatch (e, cases) ->
      let e_str = format_expr ctx e in
      let cases_str = List.map (fun (pat, body) ->
        let pat_str = format_pattern pat in
        let body_str = format_expr (indent ctx) body in
        "| " ^ pat_str ^ " -> " ^ body_str
      ) cases in
      "match " ^ e_str ^ " with\n" ^ String.concat "\n" (List.map (fmt_line (indent ctx)) cases_str)

  | ESeq (e1, e2) ->
      format_expr ctx e1 ^ ";\n" ^ fmt_line ctx (format_expr ctx e2)

  | EWhile (cond, body) ->
      let body_ctx = indent ctx in
      "while " ^ format_expr ctx cond ^ " do\n" ^
      fmt_line body_ctx (format_expr body_ctx body) ^ "\n" ^
      fmt_line ctx "done"

  | EIndex (e, idx) -> format_expr ctx e ^ ".[" ^ format_expr ctx idx ^ "]"

  | ESlice (e, start, stop) ->
      let start_str = match start with Some s -> format_expr ctx s | None -> "" in
      let stop_str = match stop with Some s -> format_expr ctx s | None -> "" in
      format_expr ctx e ^ ".[" ^ start_str ^ ":" ^ stop_str ^ "]"

  | EArrayGet (e, idx) -> format_expr ctx e ^ ".(" ^ format_expr ctx idx ^ ")"

  | ERecord fields ->
      let fields_str = List.map (fun (k, e) ->
        k ^ " = " ^ format_expr ctx e
      ) fields in
      "{" ^ String.concat "; " fields_str ^ "}"

  | ERecordGet (e, field) -> format_expr ctx e ^ "." ^ field

  | ERecordUpdate (e, fields) ->
      let fields_str = List.map (fun (k, f) ->
        k ^ " = " ^ format_expr ctx f
      ) fields in
      "{" ^ format_expr ctx e ^ " with " ^ String.concat "; " fields_str ^ "}"

  | EDot (e, field) -> format_expr ctx e ^ "." ^ field

  | ERange (e1, e2) -> format_expr ctx e1 ^ " .. " ^ format_expr ctx e2

  | EModule (name, e) ->
      let body_ctx = indent ctx in
      "module " ^ name ^ " = struct\n" ^
      fmt_line body_ctx (format_expr body_ctx e) ^ "\n" ^
      fmt_line ctx "end"

  | EModuleType (name, e) ->
      let body_ctx = indent ctx in
      "module type " ^ name ^ " = sig\n" ^
      fmt_line body_ctx (format_expr body_ctx e) ^ "\n" ^
      fmt_line ctx "end"

  | EOpen name -> "open " ^ name

  | ETypeDef (name, params, ctors) ->
      let params_str = match params with
        | [] -> ""
        | ps -> " " ^ String.concat " " ps
      in
      let ctors_str = List.map (fun (c, arg, ret) ->
        match (arg, ret) with
        | None, None -> c
        | Some a, None -> c ^ " of " ^ a
        | None, Some r -> c ^ " : " ^ r
        | Some a, Some r -> c ^ " of " ^ a ^ " : " ^ r
      ) ctors in
      "type " ^ name ^ params_str ^ " =\n" ^
      String.concat "\n" (List.map (fun s -> fmt_line (indent ctx) ("| " ^ s)) ctors_str)

  | ETraitDef (name, params, methods) ->
      let params_str = match params with
        | [] -> ""
        | ps -> "[" ^ String.concat ", " ps ^ "]"
      in
      let methods_str = List.map (fun (m, ty) -> m ^ " : " ^ ty
      ) methods in
      "trait " ^ name ^ params_str ^ " {\n" ^
      String.concat "\n" (List.map (fun s -> fmt_line (indent ctx) s) methods_str) ^ "\n" ^
      fmt_line ctx "}"

  | ETraitImpl (trait, ty, methods) ->
      let methods_str = List.map (fun (m, e) -> m ^ " = " ^ format_expr ctx e
      ) methods in
      "impl " ^ trait ^ " for " ^ ty ^ " {\n" ^
      String.concat "\n" (List.map (fun s -> fmt_line (indent ctx) s) methods_str) ^ "\n" ^
      fmt_line ctx "}"

  | EEffectDef (name, ops) ->
      let ops_str = List.map (fun op -> op) ops in
      "effect " ^ name ^ " {\n" ^
      String.concat "\n" (List.map (fun s -> fmt_line (indent ctx) s) ops_str) ^ "\n" ^
      fmt_line ctx "}"

  | EPerform (op, e) -> "perform " ^ op ^ " " ^ format_expr_atomic ctx e

  | EHandle (e, handlers) ->
      let e_str = format_expr ctx e in
      let handlers_str = List.map (fun (op, arg, k, body) ->
        let body_str = format_expr (indent (indent ctx)) body in
        fmt_line (indent ctx) (op ^ " " ^ arg ^ " " ^ k ^ " ->\n" ^ body_str)
      ) handlers in
      "handle " ^ e_str ^ " with {\n" ^
      String.concat "\n" handlers_str ^ "\n" ^
      fmt_line ctx "}"

  | ECtor (name, None) -> name
  | ECtor (name, Some e) -> name ^ " " ^ format_expr_atomic ctx e
  | ESpawn e -> "spawn " ^ format_expr_atomic ctx e
  | ESend (e1, e2) -> format_expr ctx e1 ^ " <- " ^ format_expr ctx e2
  | EReceive -> "receive"

  | ETry (e, cases) ->
      let e_str = format_expr ctx e in
      let cases_str = List.map (fun (pat, body) ->
        let pat_str = format_pattern pat in
        let body_str = format_expr (indent ctx) body in
        "| " ^ pat_str ^ " -> " ^ body_str
      ) cases in
      "try " ^ e_str ^ " with\n" ^ String.concat "\n" (List.map (fmt_line (indent ctx)) cases_str)

  | EAssign (e1, e2) -> format_expr ctx e1 ^ " <- " ^ format_expr ctx e2

and format_expr_atomic ctx expr =
  if needs_parens expr then
    "(" ^ format_expr ctx expr ^ ")"
  else
    format_expr ctx expr

and format_binary ctx op a b =
  let a_str = format_expr_atomic ctx a in
  let b_str = format_expr_atomic ctx b in
  a_str ^ " " ^ op ^ " " ^ b_str

and format_pattern = function
  | PWildcard -> "_"
  | PVar x -> x
  | PInt n -> string_of_int n
  | PBool true -> "true"
  | PBool false -> "false"
  | PString s -> "\"" ^ s ^ "\""
  | PUnit -> "()"
  | PList ps -> "[" ^ String.concat "; " (List.map format_pattern ps) ^ "]"
  | PTuple ps -> "(" ^ String.concat ", " (List.map format_pattern ps) ^ ")"
  | PRecord fields -> "{" ^ String.concat "; " (List.map (fun (k, p) -> k ^ " = " ^ format_pattern p) fields) ^ "}"
  | PCons (p1, p2) -> format_pattern p1 ^ " :: " ^ format_pattern p2
  | PCtor (name, None) -> name
  | PCtor (name, Some p) -> name ^ " " ^ format_pattern p

(** 格式化表达式为字符串 *)
let format expr =
  format_expr default_ctx expr

(** 格式化表达式列表为字符串 *)
let format_program exprs =
  String.concat "\n\n" (List.map (format_expr default_ctx) exprs)
