(** 寄存器字节码编译器

    将 AST 编译为基于寄存器的字节码指令。
*)

open Core
open Ast
open Reg_bytecode

(** 编译状态 *)
type compile_state = {
  mutable next_reg : int;
  mutable constants : const_pool list;
  mutable env : (string * int) list;
  mutable code : reg_instr list;
  mutable num_locals : int;
  mutable functions : reg_func list;
}

let fresh_state () = {
  next_reg = 0;
  constants = [];
  env = [];
  code = [];
  num_locals = 0;
  functions = [];
}

(** 收集表达式的自由变量 *)
let rec free_vars_expr bound = function
  | EVar x when not (List.mem bound x ~equal:String.equal) -> [x]
  | EInt _ | EBool _ | EChar _ | EString _ -> []
  | EList [] -> []
  | EAdd (e1, e2) | ESub (e1, e2) | EMul (e1, e2) | EDiv (e1, e2)
  | EEq (e1, e2) | ENeq (e1, e2) | ELt (e1, e2) | ELe (e1, e2)
  | EGt (e1, e2) | EGe (e1, e2) | EAnd (e1, e2) | EOr (e1, e2)
  | ECons (e1, e2) | ECat (e1, e2) | ESeq (e1, e2) | EAssign (e1, e2) ->
      free_vars_expr bound e1 @ free_vars_expr bound e2
  | ENot e | ERef e | EDeref e | ERaise e ->
      free_vars_expr bound e
  | EIf (cond, e1, e2) ->
      free_vars_expr bound cond @ free_vars_expr bound e1 @ free_vars_expr bound e2
  | EWhile (cond, body) ->
      free_vars_expr bound cond @ free_vars_expr bound body
  | EFun (param, body) ->
      free_vars_expr (param :: bound) body
  | ELet (x, e1, e2) | ELetRec (x, e1, e2) ->
      free_vars_expr bound e1 @ free_vars_expr (x :: bound) e2
  | EApp (e1, e2) ->
      free_vars_expr bound e1 @ free_vars_expr bound e2
  | EList exprs | ETuple exprs | EArray exprs ->
      List.concat_map exprs ~f:(free_vars_expr bound)
  | EMatch (e, cases) ->
      free_vars_expr bound e @ List.concat_map cases ~f:(fun (pat, body) ->
        let bound' = bound @ pattern_vars pat in
        free_vars_expr bound' body)
  | ERecord fields ->
      List.concat_map fields ~f:(fun (_, e) -> free_vars_expr bound e)
  | ERecordGet (e, _) | ERecordUpdate (e, _) ->
      free_vars_expr bound e
  | EIndex (e, _) ->
      free_vars_expr bound e
  | ESlice (e, _, _) ->
      free_vars_expr bound e
  | _ -> []

and pattern_vars = function
  | PVar x -> [x]
  | PList pats | PTuple pats -> List.concat_map pats ~f:pattern_vars
  | PCtor (_, Some pat) -> pattern_vars pat
  | _ -> []

let alloc_reg state =
  let r = state.next_reg in
  state.next_reg <- r + 1;
  r

let add_const state c =
  let idx = List.length state.constants in
  state.constants <- state.constants @ [c];
  idx

let emit state instr =
  state.code <- state.code @ [instr]

let get_code_length state = List.length state.code

let lookup_var state x =
  match List.Assoc.find state.env ~equal:String.equal x with
  | Some idx -> idx
  | None -> -1

let bind_var state x reg =
  state.env <- List.Assoc.add state.env ~equal:String.equal x reg

(** 回填跳转指令 *)
let patch_jump state jump_idx target_idx =
  let offset = target_idx - jump_idx in
  let patched = List.mapi state.code ~f:(fun i instr ->
    if i = jump_idx then
      match instr with
      | RJump _ -> RJump offset
      | RJumpIfFalse (r, _) -> RJumpIfFalse (r, offset)
      | RJumpIfTrue (r, _) -> RJumpIfTrue (r, offset)
      | _ -> instr
    else instr) in
  state.code <- patched

let rec compile_expr ?(is_tail=false) state dst = function
  | EInt n ->
      let idx = add_const state (CPInt n) in
      emit state (RLoadConst (dst, idx))
  
  | EBool b ->
      let idx = add_const state (CPBool b) in
      emit state (RLoadConst (dst, idx))
  
  | EString s ->
      let idx = add_const state (CPString s) in
      emit state (RLoadConst (dst, idx))
  
  | EList [] ->
      emit state (RLoadNil dst)
  
  
  | EVar x ->
      let reg = lookup_var state x in
      if reg >= 0 then
        emit state (RMove (dst, reg))
      else
        emit state (RLoadVar (dst, x))
  
  | EAdd (e1, e2) ->
      let r1 = alloc_reg state in
      let r2 = alloc_reg state in
      compile_expr state r1 e1;
      compile_expr state r2 e2;
      emit state (RAdd (dst, r1, r2))
  
  | ESub (e1, e2) ->
      let r1 = alloc_reg state in
      let r2 = alloc_reg state in
      compile_expr state r1 e1;
      compile_expr state r2 e2;
      emit state (RSub (dst, r1, r2))
  
  | EMul (e1, e2) ->
      let r1 = alloc_reg state in
      let r2 = alloc_reg state in
      compile_expr state r1 e1;
      compile_expr state r2 e2;
      emit state (RMul (dst, r1, r2))
  
  | EDiv (e1, e2) ->
      let r1 = alloc_reg state in
      let r2 = alloc_reg state in
      compile_expr state r1 e1;
      compile_expr state r2 e2;
      emit state (RDiv (dst, r1, r2))
  
  | EEq (e1, e2) ->
      let r1 = alloc_reg state in
      let r2 = alloc_reg state in
      compile_expr state r1 e1;
      compile_expr state r2 e2;
      emit state (REq (dst, r1, r2))
  
  | ENeq (e1, e2) ->
      let r1 = alloc_reg state in
      let r2 = alloc_reg state in
      compile_expr state r1 e1;
      compile_expr state r2 e2;
      emit state (RNeq (dst, r1, r2))
  
  | ELt (e1, e2) ->
      let r1 = alloc_reg state in
      let r2 = alloc_reg state in
      compile_expr state r1 e1;
      compile_expr state r2 e2;
      emit state (RLt (dst, r1, r2))
  
  | ELe (e1, e2) ->
      let r1 = alloc_reg state in
      let r2 = alloc_reg state in
      compile_expr state r1 e1;
      compile_expr state r2 e2;
      emit state (RLe (dst, r1, r2))
  
  | EGt (e1, e2) ->
      let r1 = alloc_reg state in
      let r2 = alloc_reg state in
      compile_expr state r1 e1;
      compile_expr state r2 e2;
      emit state (RGt (dst, r1, r2))
  
  | EGe (e1, e2) ->
      let r1 = alloc_reg state in
      let r2 = alloc_reg state in
      compile_expr state r1 e1;
      compile_expr state r2 e2;
      emit state (RGe (dst, r1, r2))
  
  | EAnd (e1, e2) ->
      let r1 = alloc_reg state in
      let r2 = alloc_reg state in
      compile_expr state r1 e1;
      compile_expr state r2 e2;
      emit state (RAnd (dst, r1, r2))
  
  | EOr (e1, e2) ->
      let r1 = alloc_reg state in
      let r2 = alloc_reg state in
      compile_expr state r1 e1;
      compile_expr state r2 e2;
      emit state (ROr (dst, r1, r2))
  
  | ENot e ->
      let r = alloc_reg state in
      compile_expr state r e;
      emit state (RNot (dst, r))
  
  | EFun (param, body) ->
      let func_state = fresh_state () in
      bind_var func_state param 0;
      func_state.next_reg <- 1;
            let body_reg = alloc_reg func_state in
            compile_expr ~is_tail:true func_state body_reg body;
            emit func_state (RReturn body_reg);
      
      let func_idx = List.length state.functions in
      let func = {
        name = "anonymous";
        params = [param];
        num_params = 1;
        constants = Array.of_list func_state.constants;
        code = Array.of_list func_state.code;
        num_locals = func_state.num_locals;
        max_regs = func_state.next_reg;
      } in
      state.functions <- state.functions @ [func];
      emit state (RLoadFunc (dst, func_idx))
  
  | EApp (EVar "print", arg) ->
      let r = alloc_reg state in
      compile_expr state r arg;
      emit state (RPrint r)
  
  | EApp (e1, e2) ->
      let r1 = alloc_reg state in
      let r2 = alloc_reg state in
      compile_expr state r1 e1;
      compile_expr state r2 e2;
      if is_tail then
        emit state (RTailCall (r1, [r2]))
      else
        emit state (RCall (dst, r1, [r2]))
  
  | EIf (cond, e1, e2) ->
      let r_cond = alloc_reg state in
      compile_expr state r_cond cond;
      
      emit state (RJumpIfFalse (r_cond, 0));
      let else_jump_idx = get_code_length state - 1 in
      
      compile_expr ~is_tail state dst e1;
      
      emit state (RJump 0);
      let end_jump_idx = get_code_length state - 1 in
      
      let else_target = get_code_length state in
      patch_jump state else_jump_idx else_target;
      
      compile_expr ~is_tail state dst e2;
      
      let end_target = get_code_length state in
      patch_jump state end_jump_idx end_target;
  
  | ELet (x, e1, e2) ->
      let r = alloc_reg state in
      compile_expr state r e1;
      bind_var state x r;
      compile_expr state dst e2

  | ELetRec (name, e1, e2) ->
      (match e1 with
       | EFun (param, body) ->
           (* 预先分配函数索引 *)
           let func_idx = List.length state.functions in
           let func_state = fresh_state () in
           (* 参数在寄存器 0 *)
           bind_var func_state param 0;
           func_state.next_reg <- 1;
           (* 将递归函数名绑定到某个寄存器，使函数体内可递归调用 *)
           let self_reg = alloc_reg func_state in
           bind_var func_state name self_reg;
           (* 加载函数闭包到 self_reg *)
           emit func_state (RLoadFunc (self_reg, func_idx));
           let body_reg = alloc_reg func_state in
           compile_expr func_state body_reg body;
           emit func_state (RReturn body_reg);
           let func = {
             name = name;
             params = [param];
             num_params = 1;
             constants = Array.of_list func_state.constants;
             code = Array.of_list func_state.code;
             num_locals = func_state.num_locals;
             max_regs = func_state.next_reg;
           } in
           state.functions <- state.functions @ [func];
           (* 在 e2 中绑定函数名 *)
           let r = alloc_reg state in
           emit state (RLoadFunc (r, func_idx));
           bind_var state name r;
           compile_expr state dst e2
       | _ ->
           let r = alloc_reg state in
           compile_expr state r e1;
           bind_var state name r;
           compile_expr state dst e2)
  
  | EList exprs ->
      let regs = List.map exprs ~f:(fun e ->
        let r = alloc_reg state in
        compile_expr state r e;
        r) in
      (match regs with
       | r :: _ -> emit state (RMove (dst, r))
       | [] -> emit state (RLoadNil dst))
  
  | ETuple exprs ->
      let regs = List.map exprs ~f:(fun e ->
        let r = alloc_reg state in
        compile_expr state r e;
        r) in
      (match regs with
       | r :: _ -> emit state (RMove (dst, r))
       | [] -> emit state (RLoadNil dst))
  
  | EMatch _ ->
      emit state (RLoadConst (dst, add_const state (CPInt 0)))
  
  | EWhile (cond, body) ->
      let loop_start = get_code_length state in
      let r_cond = alloc_reg state in
      compile_expr state r_cond cond;
      emit state (RJumpIfFalse (r_cond, 0));
      let exit_jump_idx = get_code_length state - 1 in
      let body_reg = alloc_reg state in
      compile_expr state body_reg body;
      let loop_offset = loop_start - get_code_length state in
      emit state (RJump loop_offset);
      let exit_target = get_code_length state in
      patch_jump state exit_jump_idx exit_target;
      emit state (RLoadNil dst)

  | ESeq (e1, e2) ->
      let r1 = alloc_reg state in
      compile_expr state r1 e1;
      compile_expr state dst e2

  | ERef e ->
      let r = alloc_reg state in
      compile_expr state r e;
      emit state (RMakeRef (dst, r))

  | EDeref e ->
      let r = alloc_reg state in
      compile_expr state r e;
      emit state (RDeref (dst, r))

  | EAssign (e1, e2) ->
      let r1 = alloc_reg state in
      let r2 = alloc_reg state in
      compile_expr state r1 e1;
      compile_expr state r2 e2;
      emit state (RAssignRef (r1, r2))

  | _ ->
      emit state (RLoadNil dst)

let compile_program asts =
  let state = fresh_state () in
  
  List.iter asts ~f:(fun expr ->
    let dst = alloc_reg state in
    compile_expr state dst expr;
    match expr with
    | EApp (EVar "print", _) -> ()
    | _ -> emit state (RPrint dst));
  
  emit state (RReturn 0);
  
  let main_func = {
    name = "main";
    params = [];
    num_params = 0;
    constants = Array.of_list state.constants;
    code = Array.of_list state.code;
    num_locals = state.num_locals;
    max_regs = state.next_reg;
  } in
  
  let all_functions = state.functions @ [main_func] in
  
  {
    entry_point = List.length state.functions;
    main_func = List.length state.functions;
    functions = Array.of_list all_functions;
  }
