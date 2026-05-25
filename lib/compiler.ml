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

(** 编译模式匹配的测试代码

    [compile_pattern_test ctx pat] 为模式 [pat] 生成测试和绑定代码。
    假设栈顶有待匹配的值。
    返回所有生成的 JumpIfFalse 占位位置（需要回填为下一个 case 的地址）。
    匹配成功后，该值会被 Pop。
*)
and compile_pattern_test ctx pat =
  match pat with
  | PWildcard ->
      emit ctx Pop;
      []
  | PVar x ->
      emit ctx (StoreVar x);
      []
  | PInt n ->
      emit ctx Dup;
      emit ctx (PushInt n);
      emit ctx Eq;
      let jump_pos = code_length ctx in
      emit ctx (JumpIfFalse 0);
      emit ctx Pop;
      [jump_pos]
  | PBool b ->
      emit ctx Dup;
      emit ctx (PushBool b);
      emit ctx Eq;
      let jump_pos = code_length ctx in
      emit ctx (JumpIfFalse 0);
      emit ctx Pop;
      [jump_pos]
  | PString s ->
      emit ctx Dup;
      emit ctx (PushString s);
      emit ctx Eq;
      let jump_pos = code_length ctx in
      emit ctx (JumpIfFalse 0);
      emit ctx Pop;
      [jump_pos]
  | PUnit ->
      emit ctx Dup;
      emit ctx PushUnit;
      emit ctx Eq;
      let jump_pos = code_length ctx in
      emit ctx (JumpIfFalse 0);
      emit ctx Pop;
      [jump_pos]
  | PList [] ->
      emit ctx Dup;
      emit ctx Length;
      emit ctx (PushInt 0);
      emit ctx Eq;
      let jump_pos = code_length ctx in
      emit ctx (JumpIfFalse 0);
      emit ctx Pop;
      [jump_pos]
  | PList ps ->
      let len = List.length ps in
      emit ctx Dup;
      emit ctx Length;
      emit ctx (PushInt len);
      emit ctx Eq;
      let len_jump = code_length ctx in
      emit ctx (JumpIfFalse 0);
      let element_jumps =
        List.concat (List.mapi (fun i p ->
          emit ctx Dup;
          emit ctx (PushInt i);
          emit ctx Index;
          compile_pattern_test ctx p
        ) ps)
      in
      emit ctx Pop;
      len_jump :: element_jumps
  | PTuple ps ->
      (* 元组在字节码中用列表表示 *)
      let len = List.length ps in
      emit ctx Dup;
      emit ctx Length;
      emit ctx (PushInt len);
      emit ctx Eq;
      let len_jump = code_length ctx in
      emit ctx (JumpIfFalse 0);
      let element_jumps =
        List.concat (List.mapi (fun i p ->
          emit ctx Dup;
          emit ctx (PushInt i);
          emit ctx Index;
          compile_pattern_test ctx p
        ) ps)
      in
      emit ctx Pop;
      len_jump :: element_jumps
  | PCons (p1, p2) ->
      emit ctx Dup;
      emit ctx Length;
      emit ctx (PushInt 0);
      emit ctx Gt;
      let len_jump = code_length ctx in
      emit ctx (JumpIfFalse 0);
      emit ctx Dup;
      emit ctx Head;
      let head_jumps = compile_pattern_test ctx p1 in
      emit ctx Dup;
      emit ctx Tail;
      let tail_jumps = compile_pattern_test ctx p2 in
      emit ctx Pop;
      len_jump :: (head_jumps @ tail_jumps)
  | PCtor (c, None) ->
      emit ctx Dup;
      emit ctx (TestCtor c);
      let jump_pos = code_length ctx in
      emit ctx (JumpIfFalse 0);
      emit ctx Pop;
      [jump_pos]
  | PCtor (c, Some p) ->
      emit ctx Dup;
      emit ctx (TestCtor c);
      let ctor_jump = code_length ctx in
      emit ctx (JumpIfFalse 0);
      emit ctx Dup;
      emit ctx (GetCtorArg 0);
      let arg_jumps = compile_pattern_test ctx p in
      [ctor_jump] @ arg_jumps
  | PRecord fields ->
      let field_jumps =
        List.concat (List.map (fun (name, p) ->
          emit ctx Dup;
          emit ctx (RecordGet name);
          compile_pattern_test ctx p
        ) fields)
      in
      emit ctx Pop;
      field_jumps

(** 编译模式匹配 *)
and compile_match ctx e cases =
  match cases with
  | [] ->
      (* 无匹配分支：压入 unit 作为默认结果 *)
      emit ctx PushUnit

  | (pat, body) :: rest ->
      (* 编译被匹配的表达式 *)
      compile_expr ctx e;
      (* 生成模式测试和绑定代码 *)
      let fail_jumps = compile_pattern_test ctx pat in
      (* 模式匹配成功，执行 body *)
      compile_expr ctx body;
      (* 跳转到结束 *)
      let end_jump = code_length ctx in
      emit ctx (Jump 0);
      (* 下一个 case 的开始位置 *)
      let next_pos = code_length ctx in
      (* 回填所有失败跳转到下一个 case *)
      List.iter (fun pos -> patch_instr ctx pos (JumpIfFalse next_pos)) fail_jumps;
      (* 编译下一个 case *)
      compile_match ctx e rest;
      (* 结束位置 *)
      let final_end = code_length ctx in
      patch_instr ctx end_jump (Jump final_end)

(** 编译表达式 *)
and compile_expr ctx expr =
  match expr with
  | EInt n -> emit ctx (PushInt n)
  | EBool b -> emit ctx (PushBool b)
  | EChar c -> emit ctx (PushChar c)
  | EString s -> emit ctx (PushString s)
  | EVar x -> emit ctx (LoadVar x)

  | EList [] -> emit ctx (MakeList 0)
  | EList es ->
      List.iter (compile_expr ctx) es;
      emit ctx (MakeList (List.length es))

  | ETuple [] -> emit ctx PushUnit
  | ETuple es ->
      List.iter (compile_expr ctx) es;
      emit ctx (MakeTuple (List.length es))

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

  | ESlice (e, start, end_) ->
      compile_expr ctx e;
      (match start with
       | Some s -> compile_expr ctx s
       | None -> emit ctx (PushInt 0));
      (match end_ with
       | Some e -> compile_expr ctx e
       | None -> emit ctx (PushInt (-1)));
      emit ctx Slice

  | ECtor (c, None) ->
      emit ctx (PushCtor (c, 0))

  | ECtor (c, Some e) ->
      compile_expr ctx e;
      emit ctx (PushCtor (c, 1))

  | ETypeDef _ ->
      (* 类型定义在运行时无操作 *)
      emit ctx PushUnit

  | ERef e ->
      compile_expr ctx e;
      emit ctx MakeRef

  | EDeref e ->
      compile_expr ctx e;
      emit ctx Deref

  | EAssign (e1, e2) ->
      (match e1 with
       | EArrayGet (arr, idx) ->
           compile_expr ctx arr;
           compile_expr ctx idx;
           compile_expr ctx e2;
           emit ctx ArraySet
       | ERecordGet (e, field) ->
           compile_expr ctx e;
           compile_expr ctx e2;
           emit ctx (RecordSet field)
       | _ ->
           compile_expr ctx e1;
           compile_expr ctx e2;
           emit ctx SetRef)

  | EMatch (e, cases) -> compile_match ctx e cases

  | ETry (e, cases) ->
      (* try e with cases
         
         生成的字节码结构：
         PushHandler catch_addr
         <e>
         PopHandler
         Jump end_addr
         catch_addr:
         <pattern匹配和handler执行>
         end_addr:
      *)
      let push_handler_pos = code_length ctx in
      emit ctx (PushHandler 0);
      compile_expr ctx e;
      emit ctx PopHandler;
      let jump_end_pos = code_length ctx in
      emit ctx (Jump 0);
      let catch_pos = code_length ctx in
      (* 将异常值保存到临时变量，然后进行模式匹配 *)
      emit ctx (StoreVar "__exn__");
      compile_match ctx (EVar "__exn__") cases;
      let end_pos = code_length ctx in
      patch_instr ctx push_handler_pos (PushHandler catch_pos);
      patch_instr ctx jump_end_pos (Jump end_pos)

  | ERaise e ->
      compile_expr ctx e;
      emit ctx RaiseExn

  | EAnnot (e, _) ->
      compile_expr ctx e

  | EArray es ->
      List.iter (compile_expr ctx) es;
      emit ctx (MakeArray (List.length es))

  | EArrayGet (arr, idx) ->
      compile_expr ctx arr;
      compile_expr ctx idx;
      emit ctx ArrayGet

  | ERange (start, end_) ->
      compile_expr ctx start;
      compile_expr ctx end_;
      emit ctx MakeRange

  | ERecord fields ->
      List.iter (fun (name, e) ->
        emit ctx (PushString name);
        compile_expr ctx e) fields;
      emit ctx (MakeRecord (List.length fields))

  | ERecordGet (e, field) ->
      compile_expr ctx e;
      emit ctx (RecordGet field)

  | ERecordUpdate (e, fields) ->
      compile_expr ctx e;
      emit ctx CopyRecord;
      let tmp = "__record_tmp_" ^ string_of_int (List.length ctx.code) in
      emit ctx (StoreVar tmp);
      List.iter (fun (name, expr) ->
        emit ctx (LoadVar tmp);
        compile_expr ctx expr;
        emit ctx (RecordSet name);
        emit ctx Pop
      ) fields;
      emit ctx (LoadVar tmp)

(** 编译顶层表达式 *)
let compile expr =
  let ctx = new_ctx () in
  compile_expr ctx expr;
  emit ctx Return;
  get_code ctx
