(** 基础语言模板 - AST 定义
    
    这是一个新语言的起点。根据你的需求修改这些类型定义。
*)

(** 源码位置 *)
type pos = {
  line : int;
  col : int;
}

(** 二元运算符 *)
type binary_op =
  | Add | Sub | Mul | Div
  | Eq | Neq | Lt | Le | Gt | Ge
  | And | Or

(** 一元运算符 *)
type unary_op =
  | Neg | Not

(** 模式 *)
type pattern =
  | PVar of string
  | PInt of int
  | PBool of bool
  | PWild
  | PTuple of pattern list
  | PList of pattern list

(** 表达式 *)
type expr =
  | EInt of int
  | EBool of bool
  | EString of string
  | EVar of string
  | ELet of string * expr * expr
  | EFun of string list * expr
  | EApp of expr * expr list
  | EIf of expr * expr * expr
  | EMatch of expr * (pattern * expr) list
  | EBinary of binary_op * expr * expr
  | EUnary of unary_op * expr
  | ETuple of expr list
  | EList of expr list
  | ESeq of expr * expr
  | EUnit

(** 值类型 *)
type value =
  | VInt of int
  | VBool of bool
  | VString of string
  | VList of value list
  | VTuple of value list
  | VFun of string list * expr * env
  | VBuiltin of string * (value list -> value)
  | VUnit

(** 环境 *)
and env = (string * value) list

(** 值转字符串 *)
let rec string_of_value = function
  | VInt n -> string_of_int n
  | VBool b -> string_of_bool b
  | VString s -> "\"" ^ s ^ "\""
  | VList vs -> "[" ^ String.concat ", " (List.map string_of_value vs) ^ "]"
  | VTuple vs -> "(" ^ String.concat ", " (List.map string_of_value vs) ^ ")"
  | VFun _ -> "<function>"
  | VBuiltin (name, _) -> "<builtin:" ^ name ^ ">"
  | VUnit -> "()"

(** 表达式转字符串（用于调试） *)
let rec string_of_expr = function
  | EInt n -> string_of_int n
  | EBool b -> string_of_bool b
  | EString s -> "\"" ^ s ^ "\""
  | EVar x -> x
  | ELet (x, e, body) -> "let " ^ x ^ " = " ^ string_of_expr e ^ " in " ^ string_of_expr body
  | EFun (params, body) -> "fun " ^ String.concat " " params ^ " -> " ^ string_of_expr body
  | EApp (f, args) -> string_of_expr f ^ "(" ^ String.concat ", " (List.map string_of_expr args) ^ ")"
  | EIf (c, t, e) -> "if " ^ string_of_expr c ^ " then " ^ string_of_expr t ^ " else " ^ string_of_expr e
  | EBinary (op, e1, e2) ->
      let op_str = match op with
        | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/"
        | Eq -> "=" | Neq -> "<>" | Lt -> "<" | Le -> "<=" | Gt -> ">" | Ge -> ">="
        | And -> "&&" | Or -> "||"
      in
      "(" ^ string_of_expr e1 ^ " " ^ op_str ^ " " ^ string_of_expr e2 ^ ")"
  | EUnary (op, e) ->
      let op_str = match op with Neg -> "-" | Not -> "not" in
      op_str ^ " " ^ string_of_expr e
  | _ -> "..."
