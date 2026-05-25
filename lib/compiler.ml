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

(** 编译上下文 *)
type context = {
  mutable code : instr list;
  mutable locals : string list;
}

let emit ctx instr = ctx.code <- instr :: ctx.code
let new_ctx () = { code = []; locals = [] }
let code_length ctx = List.length ctx.code

(** 尾调用优化（窥孔优化）

    扫描字节码，将连续的 Call + Return 替换为 TailCall。
    TailCall 复用当前栈帧，避免调用栈增长。
*)
let optimize_tail_calls code =
  let rec loop acc = function
    | [] -> List.rev acc
    | Bytecode.Call :: Bytecode.Return :: rest ->
        loop (Bytecode.TailCall :: acc) rest
    | h :: t -> loop (h :: acc) t
  in
  loop [] code

let get_code ctx = Array.of_list (List.rev ctx.code)

let get_code_with_opt ctx =
  Array.of_list (optimize_tail_calls (List.rev ctx.code))

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

(** 编译二元运算符 *)
let rec compile_binop ctx e1 e2 op =
  compile_expr ctx e1;
  compile_expr ctx e2;
  emit ctx op

(** 生成条件分支代码

    [emit_conditional_branch ctx ~test ~body ~rest]
    生成标准的三段式条件跳转：
      <test>
      JumpIfFalse else_label
      <body>
      Jump end_label
      else_label:
      <rest>
      end_label:
*)
and emit_conditional_branch ctx ~test ~body ~rest =
  test ();
  let jump_else_pos = code_length ctx in
  emit ctx (JumpIfFalse 0);
  body ();
  let jump_end_pos = code_length ctx in
  emit ctx (Jump 0);
  let else_pos = code_length ctx in
  rest ();
  let end_pos = code_length ctx in
  patch_instr ctx jump_else_pos (JumpIfFalse else_pos);
  patch_instr ctx jump_end_pos (Jump end_pos)

(** 编译模式匹配

    支持的模式：
    - PWildcard：通配符 _
    - PVar x：变量绑定
    - PInt n / PBool b / PString s / PUnit：常量
    - PList []：空列表
    - PCons (p1, p2)：列表解构 h :: t

    不支持的（触发编译时错误）：
    - PList (_ :: _)：非空列表字面量
    - PTuple _：元组模式
*)
and compile_match ctx e cases =
  match cases with
  | [] ->
      (* 无匹配分支：压入 unit 作为默认结果 *)
      emit ctx PushUnit

  | (PWildcard, body) :: _ ->
      compile_expr ctx e;
      emit ctx Pop;
      compile_expr ctx body

  | (PVar x, body) :: _ ->
      compile_expr ctx e;
      emit ctx (StoreVar x);
      compile_expr ctx body

  | (PInt n, body) :: rest ->
      emit_conditional_branch ctx
        ~test:(fun () ->
          compile_expr ctx e;
          emit ctx Dup;
          emit ctx (PushInt n);
          emit ctx Eq)
        ~body:(fun () -> emit ctx Pop; compile_expr ctx body)
        ~rest:(fun () -> compile_match ctx e rest)

  | (PBool b, body) :: rest ->
      emit_conditional_branch ctx
        ~test:(fun () ->
          compile_expr ctx e;
          emit ctx Dup;
          emit ctx (PushBool b);
          emit ctx Eq)
        ~body:(fun () -> emit ctx Pop; compile_expr ctx body)
        ~rest:(fun () -> compile_match ctx e rest)

  | (PString s, body) :: rest ->
      emit_conditional_branch ctx
        ~test:(fun () ->
          compile_expr ctx e;
          emit ctx Dup;
          emit ctx (PushString s);
          emit ctx Eq)
        ~body:(fun () -> emit ctx Pop; compile_expr ctx body)
        ~rest:(fun () -> compile_match ctx e rest)

  | (PUnit, body) :: rest ->
      emit_conditional_branch ctx
        ~test:(fun () ->
          compile_expr ctx e;
          emit ctx Dup;
          emit ctx PushUnit;
          emit ctx Eq)
        ~body:(fun () -> emit ctx Pop; compile_expr ctx body)
        ~rest:(fun () -> compile_match ctx e rest)

  | (PList [], body) :: rest ->
      emit_conditional_branch ctx
        ~test:(fun () ->
          compile_expr ctx e;
          emit ctx Dup;
          emit ctx Length;
          emit ctx (PushInt 0);
          emit ctx Eq)
        ~body:(fun () -> emit ctx Pop; compile_expr ctx body)
        ~rest:(fun () -> compile_match ctx e rest)

  | (PList (_ :: _), _) :: _ ->
      failwith "编译器: 非空列表模式暂不支持字节码编译"
  | (PTuple _, _) :: _ ->
      failwith "编译器: 元组模式暂不支持字节码编译"

  | (PCons (p1, p2), body) :: rest ->
      emit_conditional_branch ctx
        ~test:(fun () ->
          compile_expr ctx e;
          emit ctx Dup;
          emit ctx Length;
          emit ctx (PushInt 0);
          emit ctx Gt)
        ~body:(fun () ->
          (* 绑定 head *)
          (match p1 with
           | PWildcard -> ()
           | PVar x ->
               emit ctx Dup;
               emit ctx Head;
               emit ctx (StoreVar x)
           | _ -> failwith "编译器: cons 模式的 head 仅支持简单变量或通配符");
          (* 绑定 tail *)
          (match p2 with
           | PWildcard -> ()
           | PVar x ->
               emit ctx Dup;
               emit ctx Tail;
               emit ctx (StoreVar x)
           | _ -> failwith "编译器: cons 模式的 tail 仅支持简单变量或通配符");
          emit ctx Pop;
          compile_expr ctx body)
        ~rest:(fun () -> compile_match ctx e rest)

(** 编译表达式 *)
and compile_expr ctx expr =
  match expr with
  | EInt n -> emit ctx (PushInt n)
  | EBool b -> emit ctx (PushBool b)
  | EString s -> emit ctx (PushString s)
  | EVar x -> emit ctx (LoadVar x)

  | EList [] -> emit ctx (MakeList 0)
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
      emit_conditional_branch ctx
        ~test:(fun () -> compile_expr ctx cond)
        ~body:(fun () -> compile_expr ctx t_branch)
        ~rest:(fun () -> compile_expr ctx f_branch)

  | ELet (x, value_expr, body) ->
      compile_expr ctx value_expr;
      emit ctx (StoreVar x);
      compile_expr ctx body

  | ELetRec (f, EFun (param, body_expr), rest) ->
      let func_ctx = new_ctx () in
      compile_expr func_ctx body_expr;
      emit func_ctx Return;
      let func_code = get_code_with_opt func_ctx in
      emit ctx (MakeClosure (param, func_code, Some f));
      emit ctx (StoreVar f);
      compile_expr ctx rest

  | ELetRec (f, _, _) ->
      failwith ("编译器: let rec 后面必须是函数定义, got: " ^ f)

  | EFun (param, body) ->
      let func_ctx = new_ctx () in
      compile_expr func_ctx body;
      emit func_ctx Return;
      let func_code = get_code_with_opt func_ctx in
      emit ctx (MakeClosure (param, func_code, None))

  | EApp (e1, e2) ->
      (match e1 with
       | EVar "length" -> compile_expr ctx e2; emit ctx Length
       | EVar "head" -> compile_expr ctx e2; emit ctx Head
       | EVar "tail" -> compile_expr ctx e2; emit ctx Tail
       | EVar "print" -> compile_expr ctx e2; emit ctx Print
       | _ ->
           compile_expr ctx e2;
           compile_expr ctx e1;
           emit ctx Call)

  | ECat (e1, e2) -> compile_binop ctx e1 e2 Concat
  | ECons (e1, e2) -> compile_binop ctx e1 e2 Cons

  | EWhile (cond, body) ->
      (* while 循环编译：
         loop_label:
         <cond>
         JumpIfFalse end_label
         <body>
         Pop
         Jump loop_label
         end_label:
         PushUnit
      *)
      let loop_pos = code_length ctx in
      compile_expr ctx cond;
      let jump_end_pos = code_length ctx in
      emit ctx (JumpIfFalse 0);
      compile_expr ctx body;
      emit ctx Pop;
      emit ctx (Jump loop_pos);
      let end_pos = code_length ctx in
      patch_instr ctx jump_end_pos (JumpIfFalse end_pos);
      emit ctx PushUnit

  | EIndex (e1, e2) ->
      compile_expr ctx e1;
      compile_expr ctx e2;
      emit ctx Index

  | ESeq (e1, e2) ->
      compile_expr ctx e1;
      emit ctx Pop;
      compile_expr ctx e2

  | ESlice _ ->
      failwith "编译器: 切片暂不支持字节码编译"

  | EMatch (e, cases) -> compile_match ctx e cases

(** 编译顶层表达式 *)
let compile expr =
  let ctx = new_ctx () in
  compile_expr ctx expr;
  emit ctx Return;
  get_code ctx
