(** 所有权与借用检查器（简化版）

    类似 Rust 的所有权系统，确保内存安全：
    1. 每个值有且只有一个所有者
    2. 所有者离开作用域时，值被释放
    3. 可以创建不可变引用（&T），任意数量
    4. 可以创建可变引用（&mut T），只能有一个
    5. 引用必须有效（不能悬垂）
*)

open Core
open Ast

type ownership =
  | Owned           (* 拥有所有权 *)
  | Borrowed        (* 被借用 *)
  | MutBorrowed     (* 被可变借用 *)
  | Moved           (* 已移动 *)

type var_info = {
  name : string;
  mutable ownership : ownership;
  mutable borrow_count : int;      (* 不可变借用计数 *)
  mutable mut_borrowed : bool;     (* 是否被可变借用 *)
}

type borrow_env = {
  mutable vars : (string, var_info) Hashtbl.t;
  mutable scope_stack : string list list;  (* 作用域栈，每层记录本作用域的变量 *)
}

let create_borrow_env () = {
  vars = Hashtbl.create (module String);
  scope_stack = [];
}

let enter_scope env =
  env.scope_stack <- [] :: env.scope_stack

let exit_scope env =
  match env.scope_stack with
  | [] -> ()
  | current :: rest ->
      (* 检查离开作用域时是否有活跃借用 *)
      List.iter current ~f:(fun name ->
        match Hashtbl.find env.vars name with
        | Some info ->
            if info.borrow_count > 0 || info.mut_borrowed then
              Printf.eprintf "警告: 变量 '%s' 在借用未结束时离开作用域\n" name;
            Hashtbl.remove env.vars name
        | None -> ());
      env.scope_stack <- rest

let declare_var env name =
  let info = { name; ownership = Owned; borrow_count = 0; mut_borrowed = false } in
  Hashtbl.set env.vars ~key:name ~data:info;
  match env.scope_stack with
  | current :: rest -> env.scope_stack <- (name :: current) :: rest
  | [] -> env.scope_stack <- [[name]]

let get_var env name =
  Hashtbl.find env.vars name

exception OwnershipError of string

(** 检查表达式中的所有权使用 *)
let rec check_expr env = function
  | EVar name ->
      (match get_var env name with
       | Some info ->
           (match info.ownership with
            | Moved -> raise (OwnershipError ("使用已移动的变量: " ^ name))
            | _ -> ())
       | None -> ())
  
  | ELet (name, e1, e2) ->
      check_expr env e1;
      (* 检查 e1 是否是移动操作 *)
      (match e1 with
       | EVar src_name ->
           (match get_var env src_name with
            | Some src_info ->
                if Poly.equal src_info.ownership Owned then (
                  src_info.ownership <- Moved;
                  declare_var env name;
                  match get_var env name with
                  | Some new_info -> new_info.ownership <- Owned
                  | None -> ())
            | None -> declare_var env name)
       | _ -> declare_var env name);
      check_expr env e2
  
  | EAssign (EVar name, e) ->
      (match get_var env name with
       | Some info ->
           if info.mut_borrowed then
             raise (OwnershipError ("不能给被可变借用的变量赋值: " ^ name));
           info.ownership <- Owned
       | None -> ());
      check_expr env e
  
  | EFun (param, body) ->
      enter_scope env;
      declare_var env param;
      check_expr env body;
      exit_scope env
  
  | EApp (EVar "print", arg) ->
      (* print 是借用，不转移所有权 *)
      check_expr env arg
  
  | EApp (f, arg) ->
      check_expr env f;
      (* 函数调用转移参数所有权（简化模型） *)
      (match arg with
       | EVar name ->
           (match get_var env name with
            | Some info -> info.ownership <- Moved
            | None -> ())
       | _ -> check_expr env arg);
      check_expr env arg
  
  | EIf (cond, e1, e2) ->
      check_expr env cond;
      enter_scope env;
      check_expr env e1;
      exit_scope env;
      enter_scope env;
      check_expr env e2;
      exit_scope env
  
  | EMatch (e, cases) ->
      check_expr env e;
      List.iter cases ~f:(fun (pat, body) ->
        enter_scope env;
        check_pattern env pat;
        check_expr env body;
        exit_scope env)
  
  | EList exprs | ETuple exprs ->
      List.iter exprs ~f:(check_expr env)
  
  | ERecord fields ->
      List.iter fields ~f:(fun (_, e) -> check_expr env e)
  
  | ERecordGet (e, _) ->
      check_expr env e
  | EDot (e, _) ->
      check_expr env e
  
  | _ -> ()

and check_pattern env = function
  | PVar name -> declare_var env name
  | PList pats | PTuple pats -> List.iter pats ~f:(check_pattern env)
  | PCtor (_, Some pat) -> check_pattern env pat
  | PCtor (_, None) -> ()
  | _ -> ()

(** 检查程序 *)
let check_program asts =
  let env = create_borrow_env () in
  enter_scope env;
  List.iter asts ~f:(check_expr env);
  exit_scope env;
  Printf.printf "所有权检查通过\n"
