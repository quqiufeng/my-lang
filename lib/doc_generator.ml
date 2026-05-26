(** 文档生成器

    从 AST 提取结构信息并生成 Markdown 格式文档。
*)

open Ast

type doc_item = {
  name : string;
  kind : string;  (* function, type, trait, impl, module, effect *)
  signature : string;
  description : string;
}

type module_doc = {
  items : doc_item list;
}

let extract_doc_items expr =
  let rec extract acc = function
    | ELet (name, e, rest) ->
        let kind, sig_ = match e with
          | EFun _ -> ("function", "val " ^ name ^ " : <function>")
          | _ -> ("value", "val " ^ name ^ " : <unknown>")
        in
        let item = { name; kind; signature = sig_; description = "" } in
        extract (item :: acc) rest
    | ELetRec (name, e, rest) ->
        let sig_ = match e with
          | EFun _ -> "val " ^ name ^ " : <function>"
          | _ -> "val " ^ name ^ " : <unknown>"
        in
        let item = { name; kind = "function"; signature = sig_; description = "" } in
        extract (item :: acc) rest
    | ETypeDef (name, params, ctors) ->
        let params_str = match params with
          | [] -> ""
          | ps -> " " ^ String.concat " " ps
        in
        let ctors_str = List.map (fun (c, arg, ret) ->
          match (arg, ret) with
          | (None, None) -> c
          | (Some a, None) -> c ^ " of " ^ a
          | (None, Some r) -> c ^ " : " ^ r
          | (Some a, Some r) -> c ^ " of " ^ a ^ " : " ^ r
        ) ctors in
        let sig_ = "type " ^ name ^ params_str ^ " = " ^ String.concat " | " ctors_str in
        let item = { name; kind = "type"; signature = sig_; description = "" } in
        item :: acc
    | ETraitDef (name, params, methods) ->
        let params_str = match params with
          | [] -> ""
          | ps -> "[" ^ String.concat ", " ps ^ "]"
        in
        let sig_ = "trait " ^ name ^ params_str in
        let item = { name; kind = "trait"; signature = sig_; description = "" } in
        item :: acc
    | ETraitImpl (trait, ty, methods) ->
        let sig_ = "impl " ^ trait ^ " for " ^ ty in
        let item = { name = trait ^ " for " ^ ty; kind = "impl"; signature = sig_; description = "" } in
        item :: acc
    | EModule (name, e) ->
        let sig_ = "module " ^ name in
        let item = { name; kind = "module"; signature = sig_; description = "" } in
        let nested = extract [] e in
        item :: (List.map (fun i -> { i with name = name ^ "." ^ i.name }) nested) @ acc
    | EEffectDef (name, ops) ->
        let sig_ = "effect " ^ name ^ " { " ^ String.concat "; " ops ^ " }" in
        let item = { name; kind = "effect"; signature = sig_; description = "" } in
        item :: acc
    | ESeq (e1, e2) ->
        let acc' = extract acc e1 in
        extract acc' e2
    | _ -> acc
  in
  List.rev (extract [] expr)

let generate_markdown doc =
  let items_str = List.map (fun item ->
    Printf.sprintf "### %s\n\n**类型**: %s\n\n**签名**: `%s`\n"
      item.name item.kind item.signature
  ) doc.items in
  String.concat "\n" items_str

let generate_module_doc expr =
  let items = extract_doc_items expr in
  { items }

let generate_markdown_file filename expr =
  let doc = generate_module_doc expr in
  let header = Printf.sprintf "# %s 文档\n\n" filename in
  let body = generate_markdown doc in
  header ^ body

(** 生成函数签名列表 *)
let generate_signatures expr =
  let items = extract_doc_items expr in
  List.map (fun item -> item.signature
  ) items
