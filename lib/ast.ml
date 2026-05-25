(** 抽象语法树定义 *)

(** 源码位置 *)
type pos = { line : int; col : int }

let string_of_pos p =
  Printf.sprintf "%d:%d" p.line p.col

(** 值类型 *)
(** 构造函数定义：名称 × 可选参数类型名 *)
type ctor_def = string * string option

(** 值类型 *)
type value =
  | VInt of int
  | VBool of bool
  | VChar of char
  | VString of string
  | VList of value list
  | VTuple of value list
  | VFun of string option * string * expr * env
  | VBuiltin of string * (env -> value -> value * env)
  | VUnit
  | VCtor of string * value option  (* 构造函数值：名称 × 可选参数 *)
  | VRef of value ref  (* 引用值 *)
  | VExn of string * value option  (* 异常值：名称 × 可选参数 *)
  | VArray of value array  (* 数组值 *)
  | VRecord of (string * value ref) list  (* 记录值：字段名 × 可变值 *)

(** 模式 *)
and pattern =
  | PWildcard
  | PVar of string
  | PInt of int
  | PBool of bool
  | PString of string
  | PUnit
  | PList of pattern list
  | PTuple of pattern list
  | PRecord of (string * pattern) list  (* 记录模式: {name = p1; age = p2} *)
  | PCons of pattern * pattern
  | PCtor of string * pattern option  (* 构造函数模式：名称 × 可选子模式 *)

(** 表达式 *)
and expr =
  | EInt of int
  | EBool of bool
  | EChar of char
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
  | ECat of expr * expr
  | EMatch of expr * (pattern * expr) list
  | ESeq of expr * expr
  | EWhile of expr * expr       (* while cond do body done *)
  | EIndex of expr * expr       (* e1[e2] *)
  | ESlice of expr * expr option * expr option  (* e[start:end] *)
  | ECtor of string * expr option  (* 构造函数表达式：名称 × 可选参数 *)
  | ETypeDef of string * string list * ctor_def list  (* 类型定义：类型名 × 类型参数列表 × 构造函数列表 *)
  | ERef of expr  (* ref expr *)
  | EDeref of expr  (* !expr *)
  | EAssign of expr * expr  (* expr := expr *)
  | ETry of expr * (pattern * expr) list  (* try expr with cases *)
  | ERaise of expr           (* raise expr *)
  | EAnnot of expr * string  (* expr : type *)
  | EArray of expr list  (* [|e1; e2; ...|] *)
  | EArrayGet of expr * expr  (* arr.(idx) *)
  | ERecord of (string * expr) list  (* {field1 = e1; field2 = e2} *)
  | ERecordGet of expr * string  (* e.field *)
  | ERecordUpdate of expr * (string * expr) list  (* {r with field1 = e1} *)
  | ERange of expr * expr  (* start .. end *)

(** 环境：变量名到值的映射 *)
and env = (string * value) list

(** 将值转换为字符串 *)
let rec string_of_value = function
  | VInt n -> string_of_int n
  | VBool true -> "true"
  | VBool false -> "false"
  | VChar c -> "'" ^ String.make 1 c ^ "'"
  | VString s -> "\"" ^ s ^ "\""
  | VList vs ->
      "[" ^ String.concat "; " (List.map string_of_value vs) ^ "]"
  | VTuple vs ->
      "(" ^ String.concat ", " (List.map string_of_value vs) ^ ")"
  | VFun (Some name, _, _, _) -> "<fun " ^ name ^ ">"
  | VFun (None, _, _, _) -> "<function>"
  | VBuiltin (name, _) -> "<builtin " ^ name ^ ">"
  | VUnit -> "()"
  | VCtor (name, None) -> name
  | VCtor (name, Some v) -> name ^ " " ^ string_of_value v
  | VRef r -> "ref " ^ string_of_value !r
  | VExn (name, None) -> "Exception: " ^ name
  | VExn (name, Some v) -> "Exception: " ^ name ^ " " ^ string_of_value v
  | VArray arr -> "[|" ^ String.concat "; " (List.map string_of_value (Array.to_list arr)) ^ "|]"
  | VRecord fields ->
      "{" ^ String.concat "; " (List.map (fun (k, v) -> k ^ " = " ^ string_of_value !v) fields) ^ "}"
