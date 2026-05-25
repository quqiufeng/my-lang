(** 抽象语法树定义 *)

(** 值类型 *)
type value =
  | VInt of int
  | VBool of bool
  | VString of string
  | VList of value list
  | VTuple of value list
  | VFun of string option * string * expr * env
  | VUnit

(** 表达式 *)
and expr =
  | EInt of int
  | EBool of bool
  | EString of string
  | EList of expr list
  | ETuple of expr list
  | EVar of string
  | EAdd of expr * expr
  | ESub of expr * expr
  | EMul of expr * expr
  | EDiv of expr * expr
  | EEq of expr * expr
  | ENeq of expr * expr
  | ELt of expr * expr
  | ELe of expr * expr
  | EGt of expr * expr
  | EGe of expr * expr
  | EAnd of expr * expr
  | EOr of expr * expr
  | ENot of expr
  | EIf of expr * expr * expr
  | ELet of string * expr * expr
  | ELetRec of string * expr * expr
  | EFun of string * expr
  | EApp of expr * expr
  | ECons of expr * expr
  | ESeq of expr * expr

(** 环境：变量名到值的映射 *)
and env = (string * value) list

(** 将值转换为字符串 *)
let rec string_of_value = function
  | VInt n -> string_of_int n
  | VBool true -> "true"
  | VBool false -> "false"
  | VString s -> "\"" ^ s ^ "\""
  | VList vs ->
      "[" ^ String.concat "; " (List.map string_of_value vs) ^ "]"
  | VTuple vs ->
      "(" ^ String.concat ", " (List.map string_of_value vs) ^ ")"
  | VFun (Some name, _, _, _) -> "<fun " ^ name ^ ">"
  | VFun (None, _, _, _) -> "<function>"
  | VUnit -> "()"
