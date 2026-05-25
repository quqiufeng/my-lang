(** 字节码虚拟机

    基于栈的虚拟机，支持：
    - 常量加载（整数、布尔、字符串、unit、nil）
    - 变量存取（LoadVar/StoreVar）
    - 算术运算（Add/Sub/Mul/Div）
    - 比较运算（Eq/Neq/Lt/Le/Gt/Ge）
    - 逻辑运算（And/Or/Not）
    - 控制流（Jump/JumpIfFalse）
    - 函数调用（MakeClosure/Call/Return）
    - 列表操作（MakeList/Cons/Head/Tail/Length）
    - 字符串拼接（Concat）
    - 其他（Print/Pop/Dup）
*)

open Ast
open Bytecode

exception VMError of string

(** 运行时值类型 *)
type vm_value =
  | VInt of int
  | VBool of bool
  | VString of string
  | VUnit
  | VNil
  | VList of vm_value list
  | VClosure of (string * vm_value) list * string * instr array * string option
    (** 闭包 = 捕获环境 × 参数名 × 函数代码 × 递归自引用名 *)

let rec string_of_vm_value = function
  | VInt n -> string_of_int n
  | VBool true -> "true"
  | VBool false -> "false"
  | VString s -> "\"" ^ s ^ "\""
  | VUnit -> "()"
  | VNil -> "[]"
  | VList vs -> "[" ^ String.concat "; " (List.map string_of_vm_value vs) ^ "]"
  | VClosure _ -> "<closure>"

(** 在环境中查找变量 *)
let lookup env x =
  match List.assoc_opt x env with
  | Some v -> v
  | None -> raise (VMError ("Unbound variable: " ^ x))

(** 执行字节码主函数

    [run code] 初始化虚拟机状态（栈、环境、程序计数器、调用栈），
    然后执行给定的指令数组，最后返回栈顶值。
*)
let run code =
  (* 虚拟机核心状态 *)
  let stack = ref [] in          (* 操作栈 *)
  let env = ref [] in            (* 当前环境（变量绑定列表） *)
  let pc = ref 0 in              (* 程序计数器 *)
  let call_stack = ref [] in     (* 调用栈：保存 (返回地址, 调用者栈, 调用者环境) *)

  (** 栈操作辅助函数 *)
  let push v = stack := v :: !stack in
  let pop () =
    match !stack with
    | v :: rest -> stack := rest; v
    | [] -> raise (VMError "Stack underflow")
  in

  (** 用于从内层 [execute_block] 跳出的异常 *)
  let exception ReturnExn in

  (** 执行单条指令

      [exec_instr instr] 根据指令类型执行相应操作。
      对于 Call 指令，会递归调用 [execute_block] 执行函数体。
      对于 Return 指令，如果调用栈非空则恢复调用者状态；
      如果调用栈为空（最外层返回），抛出 [ReturnExn] 以跳出当前代码块。
  *)
  let rec exec_instr instr =
    match instr with
    | PushInt n -> push (VInt n)
    | PushBool b -> push (VBool b)
    | PushString s -> push (VString s)
    | PushUnit -> push VUnit
    | PushNil -> push VNil

    | LoadVar x -> push (lookup !env x)
    | StoreVar x -> env := (x, pop ()) :: !env

    (* 算术运算：从栈顶弹出两个操作数，计算后压入结果 *)
    | Add ->
        (match pop (), pop () with
         | VInt b, VInt a -> push (VInt (a + b))
         | _ -> raise (VMError "Type error: + requires integers"))
    | Sub ->
        (match pop (), pop () with
         | VInt b, VInt a -> push (VInt (a - b))
         | _ -> raise (VMError "Type error: - requires integers"))
    | Mul ->
        (match pop (), pop () with
         | VInt b, VInt a -> push (VInt (a * b))
         | _ -> raise (VMError "Type error: * requires integers"))
    | Div ->
        (match pop (), pop () with
         | VInt 0, VInt _ -> raise (VMError "Division by zero")
         | VInt b, VInt a -> push (VInt (a / b))
         | _ -> raise (VMError "Type error: / requires integers"))

    (* 比较运算：支持 int、bool、string 类型 *)
    | Eq ->
        (match pop (), pop () with
         | VInt b, VInt a -> push (VBool (a = b))
         | VBool b, VBool a -> push (VBool (a = b))
         | VString b, VString a -> push (VBool (a = b))
         | VUnit, VUnit -> push (VBool true)
         | _ -> push (VBool false))
    | Neq ->
        (match pop (), pop () with
         | VInt b, VInt a -> push (VBool (a <> b))
         | VBool b, VBool a -> push (VBool (a <> b))
         | VString b, VString a -> push (VBool (a <> b))
         | VUnit, VUnit -> push (VBool false)
         | _ -> push (VBool true))
    | Lt ->
        (match pop (), pop () with
         | VInt b, VInt a -> push (VBool (a < b))
         | _ -> raise (VMError "Type error: < requires integers"))
    | Le ->
        (match pop (), pop () with
         | VInt b, VInt a -> push (VBool (a <= b))
         | _ -> raise (VMError "Type error: <= requires integers"))
    | Gt ->
        (match pop (), pop () with
         | VInt b, VInt a -> push (VBool (a > b))
         | _ -> raise (VMError "Type error: > requires integers"))
    | Ge ->
        (match pop (), pop () with
         | VInt b, VInt a -> push (VBool (a >= b))
         | _ -> raise (VMError "Type error: >= requires integers"))

    (* 逻辑运算 *)
    | And ->
        (match pop (), pop () with
         | VBool b, VBool a -> push (VBool (a && b))
         | _ -> raise (VMError "Type error: && requires booleans"))
    | Or ->
        (match pop (), pop () with
         | VBool b, VBool a -> push (VBool (a || b))
         | _ -> raise (VMError "Type error: || requires booleans"))
    | Not ->
        (match pop () with
         | VBool b -> push (VBool (not b))
         | _ -> raise (VMError "Type error: not requires boolean"))

    (* 控制流 *)
    | Jump addr -> pc := addr
    | JumpIfFalse addr ->
        (match pop () with
         | VBool false -> pc := addr
         | VBool true -> ()
         | _ -> raise (VMError "Type error: if requires boolean"))

    (* 函数：创建闭包 *)
    | MakeClosure (param, func_code, self_name) ->
        (match self_name with
         | Some name ->
             (* 对于递归函数，使用 OCaml 的 let rec 创建自引用闭包 *)
             let rec self = VClosure ((name, self) :: !env, param, func_code, self_name) in
             push self
         | None ->
             push (VClosure (!env, param, func_code, self_name)))

      (* 函数调用 *)
      | Call ->
          (match pop () with
           | VClosure (closure_env, param, func_code, _) ->
               let arg = pop () in
               call_stack := (!pc, !stack, !env) :: !call_stack;
               pc := 0;
               stack := [];
               env := (param, arg) :: closure_env;
               execute_block func_code
           | _ -> raise (VMError "Type error: call requires function"))
      | TailCall ->
          (match pop () with
           | VClosure (closure_env, param, func_code, _) ->
               let arg = pop () in
               (* 尾调用：复用当前栈帧，不保存调用者状态 *)
               pc := 0;
               stack := [];
               env := (param, arg) :: closure_env;
               execute_block func_code
           | _ -> raise (VMError "Type error: call requires function"))

    (* 函数返回 *)
    | Return ->
        (match !call_stack with
         | (old_pc, old_stack, old_env) :: rest ->
             (* 获取返回值：优先取栈顶，空栈返回 unit *)
             let result =
               match !stack with
               | [v] -> v
               | [] -> VUnit
               | v :: _ -> v
             in
             (* 恢复调用者状态 *)
             pc := old_pc;
             stack := old_stack;
             env := old_env;
             call_stack := rest;
             push result
         | [] ->
             (* 最外层返回：抛出异常跳出当前代码块 *)
             raise ReturnExn)

    (* 列表操作 *)
    | MakeList n ->
        let rec loop acc n =
          if n = 0 then acc
          else loop (pop () :: acc) (n - 1)
        in
        push (VList (loop [] n))
    | Cons ->
        (match pop (), pop () with
         | VList tail, head -> push (VList (head :: tail))
         | _ -> raise (VMError "Type error: :: requires a list"))
    | Head ->
        (match pop () with
         | VList (h :: _) -> push h
         | VList [] -> raise (VMError "head: empty list")
         | _ -> raise (VMError "head: expected list"))
    | Tail ->
        (match pop () with
         | VList (_ :: t) -> push (VList t)
         | VList [] -> raise (VMError "tail: empty list")
         | _ -> raise (VMError "tail: expected list"))
    | Length ->
        (match pop () with
         | VList l -> push (VInt (List.length l))
         | VString s -> push (VInt (String.length s))
         | _ -> raise (VMError "length: expected list or string"))
    | Index ->
        (match pop (), pop () with
         | VInt idx, VList vs ->
             if idx < 0 || idx >= List.length vs then
               raise (VMError ("Index out of bounds: " ^ string_of_int idx))
             else push (List.nth vs idx)
         | VInt idx, VString s ->
             if idx < 0 || idx >= String.length s then
               raise (VMError ("String index out of bounds: " ^ string_of_int idx))
             else push (VString (String.make 1 s.[idx]))
         | _ -> raise (VMError "Type error: index requires int and list/string"))

    (* 字符串 *)
    | Concat ->
        (match pop (), pop () with
         | VString b, VString a -> push (VString (a ^ b))
         | _ -> raise (VMError "Type error: ^ requires strings"))

    (* 其他 *)
    | Print ->
        let v = pop () in
        print_endline (string_of_vm_value v);
        push VUnit
    | Pop -> ignore (pop ())
    | Dup ->
        match !stack with
        | v :: _ -> push v
        | [] -> raise (VMError "Stack underflow")

  (** 执行指令块

      [execute_block block_code] 顺序执行指令数组中的每条指令，
      直到 pc 越界或遇到 Return 指令且调用栈为空（抛出 [ReturnExn]）。
  *)
  and execute_block block_code =
    try
      while !pc < Array.length block_code do
        let instr = block_code.(!pc) in
        pc := !pc + 1;
        exec_instr instr
      done
    with ReturnExn -> ()
  in

  (* 执行主代码块 *)
  execute_block code;

  (* 返回最终栈顶值 *)
  match !stack with
  | [v] -> v
  | [] -> VUnit
  | v :: _ -> v
