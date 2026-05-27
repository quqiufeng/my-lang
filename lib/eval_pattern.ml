(** 模式匹配函数 *)

open Ast

(** 匹配单个模式，返回绑定列表或 None *)
let rec match_pattern pat value =
  match pat, value with
  | PWildcard, _ -> Some []
  | PVar x, v -> Some [(x, v)]
  | PInt n, VInt m when n = m -> Some []
  | PBool b, VBool c when b = c -> Some []
  | PString s, VString t when s = t -> Some []
  | PUnit, VUnit -> Some []
  | PList ps, VList vs when List.length ps = List.length vs ->
      match_patterns ps vs
  | PTuple ps, VTuple vs when List.length ps = List.length vs ->
      match_patterns ps vs
  | PRecord fields, VRecord record_fields ->
      let rec match_record = function
        | [] -> Some []
        | (name, pat) :: rest ->
            (match List.assoc_opt name record_fields with
             | Some ref_val ->
                 (match match_pattern pat !ref_val with
                  | Some b1 ->
                      (match match_record rest with
                       | Some b2 -> Some (b1 @ b2)
                       | None -> None)
                  | None -> None)
             | None -> None)
      in
      match_record fields
  | PCons (p1, p2), VList (h :: t) ->
      (match match_pattern p1 h with
       | Some b1 ->
           (match match_pattern p2 (VList t) with
            | Some b2 -> Some (b1 @ b2)
            | None -> None)
       | None -> None)
  | PCtor (c, None), VCtor (d, None) when c = d -> Some []
  | PCtor (c, Some p), VCtor (d, Some v) when c = d -> match_pattern p v
  | PCtor _, _ -> None
  | _ -> None

(** 匹配多个模式列表 *)
and match_patterns ps vs =
  match ps, vs with
  | [], [] -> Some []
  | p :: ps', v :: vs' ->
      (match match_pattern p v with
       | Some b1 ->
           (match match_patterns ps' vs' with
            | Some b2 -> Some (b1 @ b2)
            | None -> None)
       | None -> None)
  | _ -> None

(** 求值模式匹配表达式，需要传入 eval 函数以处理循环依赖 *)
let eval_match eval_fn env v cases =
  let rec loop = function
    | [] -> Error "匹配失败: 没有匹配的模式"
    | (p, body) :: rest ->
        (match match_pattern p v with
         | Some bindings -> eval_fn (bindings @ env) body
         | None -> loop rest)
  in
  loop cases
