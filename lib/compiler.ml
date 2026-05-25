(** AST -> 字节码编译器

    将抽象语法树编译为虚拟机指令序列。
    采用两阶段编译：
    1. 生成指令列表（支持 O(1) 尾部追加）
    2. 反转并转换为数组

    控制流（if/match）使用回填技术（backpatching）：
    先占位 Jump/JumpIfFalse 指令，待目标地址确定后再修补。
*)

open Ast
open Bytecode

(** 编译上下文

    维护当前函数的：
    - [code]：累积的指令列表（逆序，最后 Array.of_list 时反转）
    - [locals]：局部变量名列表（当前未使用，预留用于后续优化）
*)
type context = {
  mutable code : instr list;
  mutable locals : string list;
}

(** 向当前上下文追加一条指令 *)
let emit ctx instr = ctx.code <- instr :: ctx.code

(** 创建新的编译上下文 *)
let new_ctx () = { code = []; locals = [] }

(** 获取编译完成的指令数组 *)
let get_code ctx = Array.of_list (List.rev ctx.code)

(** 获取当前指令数量 *)
let code_length ctx = List.length ctx.code

(** 修改指定位置的指令（从尾部计数）

    [patch_instr ctx pos instr] 将距离当前指令列表尾部 [pos] 个位置的指令替换为 [instr]。
    用于回填跳转地址。
*)
let patch_instr ctx pos instr =
  let len = List.length ctx.code in
  let idx = len - pos - 1 in
  let rec replace i = function
    | [] -> []
    | _ :: rest when i = 0 -> instr :: rest
    | h :: rest -> h :: replace (i - 1) rest
  in
  ctx.code <- replace idx ctx.code

(** 编译二元运算符辅助函数

    先编译左操作数，再编译右操作数，最后发出运算符指令。
*)
let rec compile_binop ctx e1 e2 op =
  compile_expr ctx e1;
  compile_expr ctx e2;
  emit ctx op

(** 编译模式匹配

    当前支持的模式：
    - [PWildcard]：忽略匹配值
    - [PVar x]：绑定变量
    - [PInt n]：整数常量（生成比较和跳转链）

    注意：更复杂的模式（列表、元组、cons）当前仅在解释器中支持。
    字节码编译器对这些模式会触发编译时错误。
*)
and compile_match ctx e cases =
  match cases with
  | [] ->
      (* 无匹配分支：压入 nil（默认行为） *)
      emit ctx PushNil

  | (PWildcard, body) :: _ ->
      (* 通配符：计算被匹配值后丢弃，执行分支体 *)
      compile_expr ctx e;
      emit ctx Pop;
      compile_expr ctx body

  | (PVar x, body) :: _ ->
      (* 变量模式：绑定被匹配值到变量，执行分支体 *)
      compile_expr ctx e;
      emit ctx (StoreVar x);
      compile_expr ctx body

  | (PInt n, body) :: rest ->
      (* 整数常量模式：比较后条件跳转 *)
      compile_expr ctx e;
      emit ctx Dup;
      emit ctx (PushInt n);
      emit ctx Eq;
      let jump_else_pos = code_length ctx in
      emit ctx (JumpIfFalse 0);
      (* 匹配成功：弹出被匹配值，执行分支体 *)
      emit ctx Pop;
      compile_expr ctx body;
      (* 跳转到 match 结尾 *)
      let jump_end_pos = code_length ctx in
      emit ctx (Jump 0);
      let else_pos = code_length ctx in
      (* 编译剩余分支 *)
      compile_match ctx e rest;
      let end_pos = code_length ctx in
      (* 回填跳转地址 *)
      patch_instr ctx jump_else_pos (JumpIfFalse else_pos);
      patch_instr ctx jump_end_pos (Jump end_pos)

  | (PBool _, _) :: _ ->
      failwith "compile_match: boolean patterns not yet supported in bytecode"
  | (PString _, _) :: _ ->
      failwith "compile_match: string patterns not yet supported in bytecode"
  | (PUnit, _) :: _ ->
      failwith "compile_match: unit patterns not yet supported in bytecode"
  | (PList _, _) :: _ ->
      failwith "compile_match: list patterns not yet supported in bytecode"
  | (PTuple _, _) :: _ ->
      failwith "compile_match: tuple patterns not yet supported in bytecode"
  | (PCons _, _) :: _ ->
      failwith "compile_match: cons patterns not yet supported in bytecode"

(** 编译表达式 *)
and compile_expr ctx expr =
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
      (* 元组在运行时统一用列表表示 *)
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
      (* if 编译：
         <cond>
         JumpIfFalse else_label
         <then>
         Jump end_label
         else_label:
         <else>
         end_label:
      *)
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
      (* 递归函数：编译函数体为独立代码块，创建自引用闭包 *)
      let func_ctx = new_ctx () in
      compile_expr func_ctx body_expr;
      emit func_ctx Return;
      let func_code = get_code func_ctx in
      emit ctx (MakeClosure (param, func_code, Some f));
      emit ctx (StoreVar f);
      compile_expr ctx rest

  | ELetRec (f, _, _) ->
      failwith ("compile_expr: let rec requires a function, got: " ^ f)

  | EFun (param, body) ->
      (* 匿名函数：编译函数体为独立代码块，创建闭包 *)
      let func_ctx = new_ctx () in
      compile_expr func_ctx body;
      emit func_ctx Return;
      let func_code = get_code func_ctx in
      emit ctx (MakeClosure (param, func_code, None))

  | EApp (e1, e2) ->
      (* 函数调用：先压入参数，再压入函数，最后 Call *)
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

(** 编译顶层表达式 *)
let compile expr =
  let ctx = new_ctx () in
  compile_expr ctx expr;
  emit ctx Return;
  get_code ctx
