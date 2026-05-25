(** 类型系统定义 *)

open Ast

(** 类型变量集合 *)
module VarSet = Set.Make (Int)

(** 多态类型方案 *)
type scheme = Forall of int list * t

(** 类型 *)
and t =
  | TInt
  | TBool
  | TString
  | TUnit
  | TList of t
  | TTuple of t list
  | TArrow of t * t
  | TVar of int

(** 替换：类型变量 ID → 类型 *)
type subst = (int * t) list

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

let rec free_vars t =
  match t with
  | TInt | TBool | TString | TUnit -> VarSet.empty
  | TList t -> free_vars t
  | TTuple ts -> List.fold_left VarSet.union VarSet.empty (List.map free_vars ts)
  | TArrow (t1, t2) -> VarSet.union (free_vars t1) (free_vars t2)
  | TVar n -> VarSet.singleton n

let free_vars_scheme (Forall (vars, t)) =
  let bound = VarSet.of_list vars in
  VarSet.diff (free_vars t) bound

(** 应用替换到类型 *)
let rec apply subst t =
  match t with
  | TVar n ->
      (match List.assoc_opt n subst with
       | Some t' -> t'
       | None -> t)
  | TList t -> TList (apply subst t)
  | TTuple ts -> TTuple (List.map (apply subst) ts)
  | TArrow (t1, t2) -> TArrow (apply subst t1, apply subst t2)
  | t -> t

let apply_scheme subst (Forall (vars, t)) =
  let subst' = List.filter (fun (n, _) -> not (List.mem n vars)) subst in
  Forall (vars, apply subst' t)

(** 组合两个替换：先应用 s1，再应用 s2 *)
let compose s2 s1 =
  let s1' = List.map (fun (n, t) -> (n, apply s2 t)) s1 in
  s1' @ s2

(** occurs check：检查类型变量 n 是否出现在类型 t 中 *)
let rec occurs n t =
  match t with
  | TVar m -> n = m
  | TList t -> occurs n t
  | TTuple ts -> List.exists (occurs n) ts
  | TArrow (t1, t2) -> occurs n t1 || occurs n t2
  | _ -> false

(** 类型统一 *)
exception TypeError of string

let rec unify t1 t2 =
  match t1, t2 with
  | TInt, TInt | TBool, TBool | TString, TString | TUnit, TUnit -> []
  | TList a, TList b -> unify a b
  | TTuple as_, TTuple bs when List.length as_ = List.length bs ->
      List.fold_left2
        (fun acc a b -> compose (unify (apply acc a) (apply acc b)) acc)
        [] as_ bs
  | TArrow (a1, a2), TArrow (b1, b2) ->
      let s1 = unify a1 b1 in
      let s2 = unify (apply s1 a2) (apply s1 b2) in
      compose s2 s1
  | TVar n, t | t, TVar n ->
      if t = TVar n then []
      else if occurs n t then raise (TypeError "occurs check failed")
      else [(n, t)]
  | _ ->
      raise
        (TypeError
           (Printf.sprintf "cannot unify %s with %s" (string_of_type t1)
              (string_of_type t2)))

(** 类型变量计数器 *)
let var_counter = ref 0

let new_var () =
  incr var_counter;
  TVar !var_counter

(** 泛化：将类型中的自由变量转为多态变量 *)
let generalize env t =
  let free_in_env =
    List.fold_left
      (fun acc (_, scheme) -> VarSet.union acc (free_vars_scheme scheme))
      VarSet.empty env
  in
  let free_in_t = VarSet.diff (free_vars t) free_in_env in
  Forall (VarSet.elements free_in_t, t)

(** 实例化：将多态变量替换为新的类型变量 *)
let instantiate (Forall (vars, t)) =
  let subst = List.map (fun v -> (v, new_var ())) vars in
  apply subst t

(** 查找变量类型 *)
let lookup env x =
  match List.assoc_opt x env with
  | Some scheme -> scheme
  | None -> raise (TypeError ("Unbound variable: " ^ x))

(** 清空类型变量计数器 *)
let reset_vars () = var_counter := 0
