(** AST -> 字节码编译器 *)

open Ast
open Bytecode

(** 编译上下文 *)
type context = {
  mutable code : instr list;
  mutable locals : string list;
}

let emit ctx instr = ctx.code <- instr :: ctx.code

let new_ctx () = { code = []; locals = [] }

let get_code ctx = Array.of_list (List.rev ctx.code)

let code_length ctx = List.length ctx.code

(** 修改指定位置的指令（从尾部计数） *)
let patch_instr ctx pos instr =
  let len = List.length ctx.code in
  let idx = len - pos - 1 in
  let rec replace i = function
    | [] -> []
    | _ :: rest when i = 0 -> instr :: rest
    | h :: rest -> h :: replace (i - 1) rest
  in
  ctx.code <- replace idx ctx.code

(** 编译表达式 *)
let rec compile_expr ctx expr =
  match expr with
  | EInt n -> emit ctx (PushInt n)
  | EBool b -> emit ctx (PushBool b)
  | EString s -> emit ctx (PushString s)
  | EVar x -> emit ctx (LoadVar x)
  | EList [] -> emit ctx PushNil
  | EList es ->
      List.iter (compile_expr ctx) es;
      emit ctx (MakeList (List.length es))
  | ETuple [] -> emit ctx PushUnit
  | ETuple es ->
      List.iter (compile_expr ctx) es;
      emit ctx (MakeList (List.length es))
  | EAdd (e1, e2) -> compile_binop ctx e1 e2 Add
  | ESub (e1, e2) -> compile_binop ctx e1 e2 Sub
  | EMul (e1, e2) -> compile_binop ctx e1 e2 Mul
  | EDiv (e1, e2) -> compile_binop ctx e1 e2 Div
  | EEq (e1, e2) -> compile_binop ctx e1 e2 Eq
  | ENeq (e1, e2) -> compile_binop ctx e1 e2 Neq
  | ELt (e1, e2) -> compile_binop ctx e1 e2 Lt
  | ELe (e1, e2) -> compile_binop ctx e1 e2 Le
  | EGt (e1, e2) -> compile_binop ctx e1 e2 Gt
  | EGe (e1, e2) -> compile_binop ctx e1 e2 Ge
  | EAnd (e1, e2) -> compile_binop ctx e1 e2 And
  | EOr (e1, e2) -> compile_binop ctx e1 e2 Or
  | ENot e ->
      compile_expr ctx e;
      emit ctx Not
  | EIf (cond, t_branch, f_branch) ->
      compile_expr ctx cond;
      let jump_else_pos = code_length ctx in
      emit ctx (JumpIfFalse 0);
      compile_expr ctx t_branch;
      let jump_end_pos = code_length ctx in
      emit ctx (Jump 0);
      let else_pos = code_length ctx in
      compile_expr ctx f_branch;
      let end_pos = code_length ctx in
      patch_instr ctx jump_else_pos (JumpIfFalse else_pos);
      patch_instr ctx jump_end_pos (Jump end_pos)
  | ELet (x, value_expr, body) ->
      compile_expr ctx value_expr;
      emit ctx (StoreVar x);
      compile_expr ctx body
  | ELetRec (f, EFun (param, body_expr), rest) ->
      let func_ctx = new_ctx () in
      compile_expr func_ctx body_expr;
      emit func_ctx Return;
      let func_code = get_code func_ctx in
      emit ctx (MakeClosure (param, func_code, Some f));
      emit ctx (StoreVar f);
      compile_expr ctx rest
  | EFun (param, body) ->
      let func_ctx = new_ctx () in
      compile_expr func_ctx body;
      emit func_ctx Return;
      let func_code = get_code func_ctx in
      emit ctx (MakeClosure (param, func_code, None))
   | EApp (e1, e2) ->
      compile_expr ctx e2;
      compile_expr ctx e1;
      emit ctx Call
  | ECat (e1, e2) -> compile_binop ctx e1 e2 Concat
  | ECons (e1, e2) -> compile_binop ctx e1 e2 Cons
  | ESeq (e1, e2) ->
      compile_expr ctx e1;
      emit ctx Pop;
      compile_expr ctx e2
  | EMatch (e, cases) -> compile_match ctx e cases
  | _ -> emit ctx PushUnit

and compile_binop ctx e1 e2 op =
  compile_expr ctx e1;
  compile_expr ctx e2;
  emit ctx op

and compile_match ctx e cases =
  match cases with
  | [] -> emit ctx PushNil
  | [(PWildcard, body)] ->
      compile_expr ctx e;
      emit ctx Pop;
      compile_expr ctx body
  | (PVar x, body) :: _ ->
      compile_expr ctx e;
      emit ctx (StoreVar x);
      compile_expr ctx body
  | (PInt n, body) :: rest ->
      compile_expr ctx e;
      emit ctx (Dup);
      emit ctx (PushInt n);
      emit ctx Eq;
      let jump_else_pos = code_length ctx in
      emit ctx (JumpIfFalse 0);
      emit ctx Pop;
      compile_expr ctx body;
      let jump_end_pos = code_length ctx in
      emit ctx (Jump 0);
      let else_pos = code_length ctx in
      compile_match ctx e rest;
      let end_pos = code_length ctx in
      patch_instr ctx jump_else_pos (JumpIfFalse else_pos);
      patch_instr ctx jump_end_pos (Jump end_pos)
  | _ -> emit ctx PushNil

(** 编译顶层表达式 *)
let compile expr =
  let ctx = new_ctx () in
  compile_expr ctx expr;
  emit ctx Return;
  get_code ctx
