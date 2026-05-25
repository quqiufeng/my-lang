(** 字节码定义 *)

type instr =
  (* 常量 *)
  | PushInt of int
  | PushBool of bool
  | PushChar of char
  | PushString of string
  | PushUnit
  | PushNil
  (* 变量 *)
  | LoadVar of string
  | StoreVar of string
  (* 算术 *)
  | Add
  | Sub
  | Mul
  | Div
  (* 比较 *)
  | Eq
  | Neq
  | Lt
  | Le
  | Gt
  | Ge
  (* 逻辑 *)
  | And
  | Or
  | Not
  (* 控制流 *)
  | Jump of int
  | JumpIfFalse of int
  (* 函数 *)
  | MakeClosure of string * code * string option
  | Call
  | TailCall
  | Return
  (* 列表 *)
  | MakeList of int
  | Cons
  | Head
  | Tail
  | Length
  | Index
  (* 字符串 *)
  | Concat
  (* ADT *)
  | PushCtor of string * int
  | TestCtor of string
  | GetCtorArg of int
  (* 引用 *)
  | MakeRef
  | Deref
  | SetRef
  | Print
  | Pop
  | Dup

and code = instr array

let rec string_of_instr = function
  | PushInt n -> Printf.sprintf "PushInt %d" n
  | PushBool true -> "PushBool true"
  | PushBool false -> "PushBool false"
  | PushChar c -> Printf.sprintf "PushChar '%c'" c
  | PushString s -> Printf.sprintf "PushString \"%s\"" s
  | PushUnit -> "PushUnit"
  | PushNil -> "PushNil"
  | LoadVar x -> Printf.sprintf "LoadVar %s" x
  | StoreVar x -> Printf.sprintf "StoreVar %s" x
  | Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div"
  | Eq -> "Eq"
  | Neq -> "Neq"
  | Lt -> "Lt"
  | Le -> "Le"
  | Gt -> "Gt"
  | Ge -> "Ge"
  | And -> "And"
  | Or -> "Or"
  | Not -> "Not"
  | Jump n -> Printf.sprintf "Jump %d" n
  | JumpIfFalse n -> Printf.sprintf "JumpIfFalse %d" n
  | MakeClosure (param, _, _) -> Printf.sprintf "MakeClosure %s" param
  | Call -> "Call"
  | TailCall -> "TailCall"
  | Return -> "Return"
  | MakeList n -> Printf.sprintf "MakeList %d" n
  | Cons -> "Cons"
  | Head -> "Head"
  | Tail -> "Tail"
  | Length -> "Length"
  | Index -> "Index"
  | Concat -> "Concat"
  | PushCtor (name, arity) -> Printf.sprintf "PushCtor %s %d" name arity
  | TestCtor name -> Printf.sprintf "TestCtor %s" name
  | GetCtorArg n -> Printf.sprintf "GetCtorArg %d" n
  | MakeRef -> "MakeRef"
  | Deref -> "Deref"
  | SetRef -> "SetRef"
  | Print -> "Print"
  | Pop -> "Pop"
  | Dup -> "Dup"

let print_code code =
  Array.iteri
    (fun i instr -> Printf.printf "%04d: %s\n" i (string_of_instr instr))
    code
