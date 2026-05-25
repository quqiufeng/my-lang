(** Traits（类型类）系统

    类似 Rust trait / Haskell Typeclass 的接口抽象。
    
    语法：
    ```
    trait Show {
      show : Self -> string
    }
    
    impl Show for int {
      let show = fun x -> string_of_int x
    }
    
    let print_show = fun x -> print (show x)
    (* print_show : Show a => a -> unit *)
    ```
*)

open Core
open Types

(** Trait 定义 *)
type trait_def = {
  trait_name : string;
  type_params : string list;  (* 类型参数，如 ['a] *)
  methods : (string * t) list;  (* 方法名 × 类型签名 *)
}

(** Trait 实现 *)
type trait_impl = {
  impl_trait : string;  (* trait 名称 *)
  impl_type : t;        (* 实现的类型 *)
  method_impls : (string * Ast.expr) list;  (* 方法名 × 实现表达式 *)
}

(** Trait 环境 *)
type trait_env = {
  mutable defs : (string, trait_def) Hashtbl.t;
  mutable impls : (string * t, trait_impl) Hashtbl.t;  (* (trait_name, type) -> impl *)
}

let create_trait_env () = {
  defs = Hashtbl.create (module String);
  impls = Hashtbl.create (module struct
    type t = string * Types.t
    let compare (n1, t1) (n2, t2) =
      let c = String.compare n1 n2 in
      if c <> 0 then c else Poly.compare (t1 : Types.t) (t2 : Types.t)
    let sexp_of_t (n, t) = Sexp.List [Sexp.Atom n; Sexp.Atom (Types.string_of_type t)]
    let t_of_sexp s = failwith "not implemented"
    let hash (n, t) = String.hash n + Hashtbl.hash (Types.string_of_type t)
  end);
}

(** 注册 trait 定义 *)
let define_trait env def =
  Hashtbl.set env.defs ~key:def.trait_name ~data:def

(** 注册 trait 实现 *)
let add_impl env impl =
  Hashtbl.set env.impls ~key:(impl.impl_trait, impl.impl_type) ~data:impl

(** 查找 trait 定义 *)
let find_trait env name =
  Hashtbl.find env.defs name

(** 查找 trait 实现 *)
let find_impl env trait_name typ =
  Hashtbl.find env.impls (trait_name, typ)

(** 内置 Traits *)

let show_trait = {
  trait_name = "Show";
  type_params = ["Self"];
  methods = [("show", TArrow (TVar 0, TString))];
}

let eq_trait = {
  trait_name = "Eq";
  type_params = ["Self"];
  methods = [
    ("eq", TArrow (TVar 0, TArrow (TVar 0, TBool)));
    ("neq", TArrow (TVar 0, TArrow (TVar 0, TBool)));
  ];
}

let ord_trait = {
  trait_name = "Ord";
  type_params = ["Self"];
  methods = [
    ("lt", TArrow (TVar 0, TArrow (TVar 0, TBool)));
    ("le", TArrow (TVar 0, TArrow (TVar 0, TBool)));
    ("gt", TArrow (TVar 0, TArrow (TVar 0, TBool)));
    ("ge", TArrow (TVar 0, TArrow (TVar 0, TBool)));
  ];
}

(** 创建包含内置 traits 的环境 *)
let builtin_traits () =
  let env = create_trait_env () in
  define_trait env show_trait;
  define_trait env eq_trait;
  define_trait env ord_trait;
  env

(** 为类型生成默认实现 *)
let add_default_impls env =
  (* int 的 Show *)
  add_impl env {
    impl_trait = "Show";
    impl_type = TInt;
    method_impls = [("show", Ast.EFun ("x", Ast.EApp (Ast.EVar "string_of_int", Ast.EVar "x")))];
  };
  (* bool 的 Show *)
  add_impl env {
    impl_trait = "Show";
    impl_type = TBool;
    method_impls = [("show", Ast.EFun ("x", Ast.EApp (Ast.EVar "string_of_bool", Ast.EVar "x")))];
  };
  (* int 的 Eq *)
  add_impl env {
    impl_trait = "Eq";
    impl_type = TInt;
    method_impls = [
      ("eq", Ast.EFun ("x", Ast.EFun ("y", Ast.EEq (Ast.EVar "x", Ast.EVar "y"))));
      ("neq", Ast.EFun ("x", Ast.EFun ("y", Ast.ENeq (Ast.EVar "x", Ast.EVar "y"))));
    ];
  }

(** 解析 trait 约束字符串，如 "Show a =>" *)
let parse_constraint s =
  let parts = String.split s ~on:' ' in
  match parts with
  | trait :: type_param :: _ -> Some (trait, type_param)
  | _ -> None

(** 检查类型是否实现了某个 trait *)
let rec implements_trait env trait_name = function
  | TVar _ -> true  (* 类型变量暂时假设满足所有 trait（需后续约束检查） *)
  | typ -> Option.is_some (find_impl env trait_name typ)

(** 获取 trait 方法的类型签名 *)
let get_method_type env trait_name method_name =
  match find_trait env trait_name with
  | Some trait -> List.Assoc.find trait.methods ~equal:String.equal method_name
  | None -> None
