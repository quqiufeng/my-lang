(** 抽象语法树定义 *)

(** 值类型 *)
type value =
  | VInt of int
  | VBool of bool
  | VFun of string * expr * env
  | VUnit

(** 表达式 *)
and expr =
  | EInt of int
  | EBool of bool
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
  | EFun of string * expr
  | EApp of expr * expr

(** 环境：变量名到值的映射 *)
and env = (string * value) list

(** 将值转换为字符串 *)
let rec string_of_value = function
  | VInt n -> string_of_int n
  | VBool true -> "true"
  | VBool false -> "false"
  | VFun _ -> "<function>"
  | VUnit -> "()"
