(** 类型系统定义

    定义 MyLang 的类型表示、替换操作和核心类型算法。
    
    核心数据类型 [t] 包括：
    - 基本类型：int, bool, string, unit
    - 复合类型：list, tuple, arrow（函数类型）
    - 类型变量：TVar（用于类型推断中的未知类型）

    替换（substitution）使用 [Int.Map] 实现，提供 O(log n) 的查找性能。
*)

open Ast

(** 类型变量集合（用于自由变量计算） *)
module VarSet = Set.Make (Int)

(** 多态类型方案

    [Forall (vars, t)] 表示类型 [t] 中 [vars] 这些类型变量是多态的
    （即可以被实例化为任意类型）。
*)
type scheme = Forall of int list * t

(** 类型表示 *)
and t =
  | TInt           (** 整数类型 *)
  | TBool          (** 布尔类型 *)
  | TString        (** 字符串类型 *)
  | TUnit          (** 单元类型 *)
  | TList of t     (** 列表类型 [t list] *)
  | TTuple of t list  (** 元组类型 [(t1, t2, ...)] *)
  | TArrow of t * t   (** 函数类型 [t1 -> t2] *)
  | TVar of int    (** 类型变量（用整数 ID 标识） *)

(** 替换：类型变量 ID -> 类型

    使用 [Map.Make(Int)] 提供高效的替换查找和组合。
*)
module Subst = Map.Make (Int)
type subst = t Subst.t

(** 将类型转换为可读字符串 *)
let rec string_of_type = function
  | TInt -> "int"
  | TBool -> "bool"
  | TString -> "string"
  | TUnit -> "unit"
  | TList t -> string_of_type t ^ " list"
  | TTuple [] -> "unit"
  | TTuple ts -> "(" ^ String.concat " * " (List.map string_of_type ts) ^ ")"
  | TArrow (t1, t2) ->
      let s1 =
        match t1 with
        | TArrow _ -> "(" ^ string_of_type t1 ^ ")"
        | _ -> string_of_type t1
      in
      s1 ^ " -> " ^ string_of_type t2
  | TVar n -> "'t" ^ string_of_int n

(** 计算类型中的自由变量 *)
let rec free_vars t =
  match t with
  | TInt | TBool | TString | TUnit -> VarSet.empty
  | TList t -> free_vars t
  | TTuple ts -> List.fold_left VarSet.union VarSet.empty (List.map free_vars ts)
  | TArrow (t1, t2) -> VarSet.union (free_vars t1) (free_vars t2)
  | TVar n -> VarSet.singleton n

(** 计算类型方案中的自由变量

    多态绑定的变量不算自由变量。
*)
let free_vars_scheme (Forall (vars, t)) =
  let bound = VarSet.of_list vars in
  VarSet.diff (free_vars t) bound

(** 应用替换到类型

    将类型中所有出现在替换域中的类型变量替换为对应类型。
*)
let rec apply subst t =
  match t with
  | TVar n ->
      (match Subst.find_opt n subst with
       | Some t' -> t'
       | None -> t)
  | TList t -> TList (apply subst t)
  | TTuple ts -> TTuple (List.map (apply subst) ts)
  | TArrow (t1, t2) -> TArrow (apply subst t1, apply subst t2)
  | t -> t

(** 应用替换到类型方案

    替换中涉及多态变量的部分被过滤掉。
*)
let apply_scheme subst (Forall (vars, t)) =
  let subst' = Subst.filter (fun n _ -> not (List.mem n vars)) subst in
  Forall (vars, apply subst' t)

(** 组合两个替换

    [compose s2 s1] 表示先应用 [s1]，再应用 [s2] 的组合替换。
    即 [apply (compose s2 s1) t = apply s2 (apply s1 t)]。
*)
let compose s2 s1 =
  let s1' = Subst.map (fun t -> apply s2 t) s1 in
  Subst.merge
    (fun _ v1 v2 ->
      match v1, v2 with
      | Some v, _ -> Some v
      | _, Some v -> Some v
      | _ -> None)
    s1' s2

(** Occurs check

    检查类型变量 [n] 是否出现在类型 [t] 中。
    用于防止构造循环类型（如 [t = t list]）。
*)
let rec occurs n t =
  match t with
  | TVar m -> n = m
  | TList t -> occurs n t
  | TTuple ts -> List.exists (occurs n) ts
  | TArrow (t1, t2) -> occurs n t1 || occurs n t2
  | _ -> false

(** 类型错误异常 *)
exception TypeError of string

(** 类型统一（Unification）

    [unify t1 t2] 计算一个替换，使得 [apply subst t1 = apply subst t2]。
    如果两个类型无法统一，抛出 [TypeError]。
*)
let rec unify t1 t2 =
  match t1, t2 with
  | TInt, TInt | TBool, TBool | TString, TString | TUnit, TUnit -> Subst.empty
  | TList a, TList b -> unify a b
  | TTuple as_, TTuple bs when List.length as_ = List.length bs ->
      List.fold_left2
        (fun acc a b -> compose (unify (apply acc a) (apply acc b)) acc)
        Subst.empty as_ bs
  | TArrow (a1, a2), TArrow (b1, b2) ->
      let s1 = unify a1 b1 in
      let s2 = unify (apply s1 a2) (apply s1 b2) in
      compose s2 s1
  | TVar n, t | t, TVar n ->
      if t = TVar n then Subst.empty
      else if occurs n t then raise (TypeError "循环类型错误")
      else Subst.singleton n t
  | _ ->
      raise
        (TypeError
           (Printf.sprintf "cannot unify %s with %s" (string_of_type t1)
              (string_of_type t2)))

(** 类型变量计数器

    用于生成唯一的类型变量 ID。
    注意：每次类型检查前应调用 [reset_vars ()] 重置。
*)
let var_counter = ref 0

(** 生成新的类型变量 *)
let new_var () =
  incr var_counter;
  TVar !var_counter

(** 泛化（Generalization）

    将类型 [t] 中不在环境 [env] 中出现的自由变量转为多态变量，
    生成类型方案 [Forall (vars, t)]。

    这是 let-多态的核心：只有不在当前环境中自由出现的类型变量才能被泛化。
*)
let generalize env t =
  let free_in_env =
    List.fold_left
      (fun acc (_, scheme) -> VarSet.union acc (free_vars_scheme scheme))
      VarSet.empty env
  in
  let free_in_t = VarSet.diff (free_vars t) free_in_env in
  Forall (VarSet.elements free_in_t, t)

(** 实例化（Instantiation）

    将类型方案中的多态变量替换为新的类型变量。
    这允许同一多态类型在不同使用点具有不同的具体类型。
*)
let instantiate (Forall (vars, t)) =
  let subst = List.fold_left (fun acc v -> Subst.add v (new_var ()) acc) Subst.empty vars in
  apply subst t

(** 在环境中查找变量类型 *)
let lookup env x =
  match List.assoc_opt x env with
  | Some scheme -> scheme
  | None -> raise (TypeError ("未绑定变量: " ^ x))

(** 重置类型变量计数器 *)
let reset_vars () = var_counter := 0
