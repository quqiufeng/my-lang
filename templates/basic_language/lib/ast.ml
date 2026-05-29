type binop =
  | Add | Sub | Mul | Div | Mod
  | Eq | Neq | Lt | Gt | Le | Ge
  | And | Or
  | LShift | RShift | BitAnd | BitXor | BitOr

type unaryop = Not | Neg | BitNot

type typ =
  | TName of string
  | TFun of typ * typ
  | TGeneric of string * typ list

type pattern =
  | PInt of int
  | PFloat of float
  | PString of string
  | PChar of char
  | PBool of bool
  | PVar of string
  | PWildcard
  | PVariant of string * pattern list
  | PTuple of pattern list
  | POr of pattern * pattern
  | PWhen of pattern * expr

and expr =
  | Int of int
  | Float of float
  | String of string
  | Char of char
  | Bool of bool
  | Var of string
  | BinOp of binop * expr * expr
  | UnaryOp of unaryop * expr
  | Call of expr * expr list
  | Field of expr * string
  | Index of expr * expr
  | Array of expr list
  | RecordLit of (string * expr) list
  | Tuple of expr list
  | Lambda of (string * typ) list * expr
  | If of expr * stmt list * stmt list option
  | While of expr * stmt list
  | For of stmt * expr * stmt * stmt list
  | Match of expr * (pattern * expr) list
  | LetIn of string * expr * expr
  | LetInTyped of string * typ * expr * expr
  | ModuleAccess of expr * string

and stmt =
  | Let of string * expr
  | LetTyped of string * typ * expr
  | FunDef of string * (string * typ) list * typ * stmt list
  | TypeAlias of string * typ
  | Variant of string * (string * typ option) list
  | Record of string * (string * typ) list
  | Module of string * stmt list
  | ModuleSig of string * stmt list
  | Import of string
  | Export of string
  | ExprStmt of expr

type program = stmt list

(** 值类型 *)
type value =
  | VInt of int
  | VFloat of float
  | VString of string
  | VBool of bool
  | VChar of char
  | VUnit

let string_of_value = function
  | VInt n -> string_of_int n
  | VFloat f -> string_of_float f
  | VString s -> "\"" ^ s ^ "\""
  | VBool true -> "true"
  | VBool false -> "false"
  | VChar c -> "'" ^ String.make 1 c ^ "'"
  | VUnit -> "()"
