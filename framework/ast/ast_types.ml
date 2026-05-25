(** 通用 AST 类型定义

    所有语言共享的通用 AST 节点类型。
    具体语言可以扩展这些类型，但元编程系统基于这些通用类型工作。
*)

(** 字面量 *)
type literal =
  | LInt of int
  | LFloat of float
  | LBool of bool
  | LString of string
  | LChar of char
  | LUnit

(** 一元运算符 *)
type unary_op =
  | Neg       (* - *)
  | Not       (* not / ! *)
  | Deref     (* ! / * *)

(** 二元运算符 *)
type binary_op =
  | Add | Sub | Mul | Div | Mod
  | Eq | Neq | Lt | Le | Gt | Ge
  | And | Or
  | Cons      (* :: *)
  | Concat    (* ^ *)

(** 模式 *)
type pattern =
  | PWildcard
  | PVar of string
  | PLit of literal
  | PTuple of pattern list
  | PList of pattern list
  | PCons of pattern * pattern
  | PRecord of (string * pattern) list
  | PCtor of string * pattern option

(** 通用 AST 节点

    这是元编程的基础：任何语言的 AST 都可以映射到这些通用节点。
*)
type expr =
  (* 字面量 *)
  | ELit of literal
  | EVar of string
  
  (* 运算符 *)
  | EUnary of unary_op * expr
  | EBinary of binary_op * expr * expr
  
  (* 绑定 *)
  | ELet of string * expr * expr
  | ELetRec of string * expr * expr
  | EAssign of expr * expr
  
  (* 控制流 *)
  | EIf of expr * expr * expr
  | EMatch of expr * (pattern * expr) list
  | EWhile of expr * expr
  | ESeq of expr * expr
  
  (* 函数 *)
  | EFun of string list * expr
  | EApp of expr * expr list
  
  (* 复合类型 *)
  | EList of expr list
  | ETuple of expr list
  | ERecord of (string * expr) list
  | EArray of expr list
  
  (* 引用 *)
  | ERef of expr
  | EDeref of expr
  
  (* 异常 *)
  | ETry of expr * (pattern * expr) list
  | ERaise of expr
  
  (* 类型 *)
  | ETypeDef of string * string list * ctor_def list
  | ECtor of string * expr option
  | EAnnot of expr * string
  
  (* 元编程 - Quote/Anti-quote *)
  | EQuote of expr          (* quote expr *)
  | EAntiQuote of expr      (* ~expr 在 quote 中 *)
  | EMacro of string * expr list  (* macro_name(args...) *)
  
  (* 模块 *)
  | EModule of string * expr
  | EOpen of string
  | EDot of expr * string

and ctor_def = string * string option

(** 将通用 AST 转为字符串（用于调试） *)
let rec string_of_expr = function
  | ELit (LInt n) -> string_of_int n
  | ELit (LFloat f) -> string_of_float f
  | ELit (LBool b) -> string_of_bool b
  | ELit (LString s) -> "\"" ^ s ^ "\""
  | ELit (LChar c) -> "'" ^ String.make 1 c ^ "'"
  | ELit LUnit -> "()"
  | EVar x -> x
  | EUnary (Neg, e) -> "-" ^ string_of_expr e
  | EUnary (Not, e) -> "not " ^ string_of_expr e
  | EUnary (Deref, e) -> "!" ^ string_of_expr e
  | EBinary (op, e1, e2) ->
      let op_str = match op with
        | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/" | Mod -> "%"
        | Eq -> "=" | Neq -> "<>" | Lt -> "<" | Le -> "<=" | Gt -> ">" | Ge -> ">="
        | And -> "&&" | Or -> "||"
        | Cons -> "::" | Concat -> "^"
      in
      "(" ^ string_of_expr e1 ^ " " ^ op_str ^ " " ^ string_of_expr e2 ^ ")"
  | ELet (x, v, body) -> "let " ^ x ^ " = " ^ string_of_expr v ^ " in " ^ string_of_expr body
  | ELetRec (x, v, body) -> "let rec " ^ x ^ " = " ^ string_of_expr v ^ " in " ^ string_of_expr body
  | EAssign (e1, e2) -> string_of_expr e1 ^ " := " ^ string_of_expr e2
  | EIf (c, t, f) -> "if " ^ string_of_expr c ^ " then " ^ string_of_expr t ^ " else " ^ string_of_expr f
  | EMatch (e, cases) -> "match " ^ string_of_expr e ^ " with ..."
  | EWhile (c, b) -> "while " ^ string_of_expr c ^ " do ... done"
  | ESeq (e1, e2) -> string_of_expr e1 ^ "; " ^ string_of_expr e2
  | EFun (params, body) -> "fun " ^ String.concat " " params ^ " -> " ^ string_of_expr body
  | EApp (f, args) -> string_of_expr f ^ " " ^ String.concat " " (List.map string_of_expr args)
  | EList es -> "[" ^ String.concat ", " (List.map string_of_expr es) ^ "]"
  | ETuple es -> "(" ^ String.concat ", " (List.map string_of_expr es) ^ ")"
  | ERecord fields -> "{" ^ String.concat "; " (List.map (fun (k, v) -> k ^ " = " ^ string_of_expr v) fields) ^ "}"
  | EArray es -> "[|" ^ String.concat "; " (List.map string_of_expr es) ^ "|]"
  | ERef e -> "ref " ^ string_of_expr e
  | EDeref e -> "!" ^ string_of_expr e
  | ETry (e, _) -> "try " ^ string_of_expr e ^ " with ..."
  | ERaise e -> "raise " ^ string_of_expr e
  | ETypeDef (name, _, _) -> "type " ^ name ^ " = ..."
  | ECtor (c, None) -> c
  | ECtor (c, Some e) -> c ^ " " ^ string_of_expr e
  | EAnnot (e, t) -> string_of_expr e ^ " : " ^ t
  | EQuote e -> "quote (" ^ string_of_expr e ^ ")"
  | EAntiQuote e -> "~(" ^ string_of_expr e ^ ")"
  | EMacro (name, args) -> "macro:" ^ name ^ "(" ^ String.concat ", " (List.map string_of_expr args) ^ ")"
  | EModule (name, _) -> "module " ^ name ^ " = ..."
  | EOpen name -> "open " ^ name
  | EDot (e, field) -> string_of_expr e ^ "." ^ field
