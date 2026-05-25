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
  | VChar of char
  | VString of string
  | VUnit
  | VNil
  | VList of vm_value list
  | VClosure of (string * vm_value) list * string * instr array * string option
    (** 闭包 = 捕获环境 × 参数名 × 函数代码 × 递归自引用名 *)
  | VCtor of string * vm_value list
    (** 代数数据类型构造函数 = 名称 × 参数列表 *)
  | VRef of vm_value ref
    (** 引用值 *)

(** 获取 VM 值的类型描述（用于错误报告） *)
let rec type_of_vm_value = function
  | VInt _ -> "int"
  | VBool _ -> "bool"
  | VChar _ -> "char"
  | VString _ -> "string"
  | VUnit -> "unit"
  | VNil -> "nil"
  | VList _ -> "list"
  | VClosure _ -> "function"
  | VCtor (name, _) -> name
  | VRef _ -> "ref"

let rec string_of_vm_value = function
  | VInt n -> string_of_int n
  | VBool true -> "true"
  | VBool false -> "false"
  | VChar c -> "'" ^ String.make 1 c ^ "'"
  | VString s -> "\"" ^ s ^ "\""
  | VUnit -> "()"
  | VNil -> "[]"
  | VList vs -> "[" ^ String.concat "; " (List.map string_of_vm_value vs) ^ "]"
  | VClosure _ -> "<closure>"
  | VCtor (name, []) -> name
  | VCtor (name, [v]) -> name ^ " " ^ string_of_vm_value v
  | VCtor (name, vs) -> name ^ " (" ^ String.concat ", " (List.map string_of_vm_value vs) ^ ")"
  | VRef r -> "ref " ^ string_of_vm_value !r

(** 在环境中查找变量 *)
let lookup env x =
  match List.assoc_opt x env with
  | Some v -> v
  | None -> raise (VMError ("未绑定变量: " ^ x))

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
    | [] -> raise (VMError "栈下溢")
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
    | PushChar c -> push (VChar c)
    | PushString s -> push (VString s)
    | PushUnit -> push VUnit
    | PushNil -> push VNil

    | LoadVar x -> push (lookup !env x)
    | StoreVar x -> env := (x, pop ()) :: !env

    (* 算术运算：从栈顶弹出两个操作数，计算后压入结果 *)
    | Add ->
        (match pop (), pop () with
         | VInt b, VInt a -> push (VInt (a + b))
         | VInt _, v2 -> raise (VMError ("类型错误: + 的右操作数是 " ^ type_of_vm_value v2 ^ "，需要整数"))
         | v1, _ -> raise (VMError ("类型错误: + 的左操作数是 " ^ type_of_vm_value v1 ^ "，需要整数")))
    | Sub ->
        (match pop (), pop () with
         | VInt b, VInt a -> push (VInt (a - b))
         | VInt _, v2 -> raise (VMError ("类型错误: - 的右操作数是 " ^ type_of_vm_value v2 ^ "，需要整数"))
         | v1, _ -> raise (VMError ("类型错误: - 的左操作数是 " ^ type_of_vm_value v1 ^ "，需要整数")))
    | Mul ->
        (match pop (), pop () with
         | VInt b, VInt a -> push (VInt (a * b))
         | VInt _, v2 -> raise (VMError ("类型错误: * 的右操作数是 " ^ type_of_vm_value v2 ^ "，需要整数"))
         | v1, _ -> raise (VMError ("类型错误: * 的左操作数是 " ^ type_of_vm_value v1 ^ "，需要整数")))
    | Div ->
        (match pop (), pop () with
         | VInt 0, VInt _ -> raise (VMError "除零错误")
         | VInt b, VInt a -> push (VInt (a / b))
         | VInt _, v2 -> raise (VMError ("类型错误: / 的右操作数是 " ^ type_of_vm_value v2 ^ "，需要整数"))
         | v1, _ -> raise (VMError ("类型错误: / 的左操作数是 " ^ type_of_vm_value v1 ^ "，需要整数")))

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
         | v1, v2 -> raise (VMError ("类型错误: < 的操作数是 " ^ type_of_vm_value v1 ^ " 和 " ^ type_of_vm_value v2 ^ "，需要整数")))
    | Le ->
        (match pop (), pop () with
         | VInt b, VInt a -> push (VBool (a <= b))
         | v1, v2 -> raise (VMError ("类型错误: <= 的操作数是 " ^ type_of_vm_value v1 ^ " 和 " ^ type_of_vm_value v2 ^ "，需要整数")))
    | Gt ->
        (match pop (), pop () with
         | VInt b, VInt a -> push (VBool (a > b))
         | v1, v2 -> raise (VMError ("类型错误: > 的操作数是 " ^ type_of_vm_value v1 ^ " 和 " ^ type_of_vm_value v2 ^ "，需要整数")))
    | Ge ->
        (match pop (), pop () with
         | VInt b, VInt a -> push (VBool (a >= b))
         | v1, v2 -> raise (VMError ("类型错误: >= 的操作数是 " ^ type_of_vm_value v1 ^ " 和 " ^ type_of_vm_value v2 ^ "，需要整数")))

    (* 逻辑运算 *)
    | And ->
        (match pop (), pop () with
         | VBool b, VBool a -> push (VBool (a && b))
         | v1, v2 -> raise (VMError ("类型错误: && 的操作数是 " ^ type_of_vm_value v1 ^ " 和 " ^ type_of_vm_value v2 ^ "，需要布尔值")))
    | Or ->
        (match pop (), pop () with
         | VBool b, VBool a -> push (VBool (a || b))
         | v1, v2 -> raise (VMError ("类型错误: || 的操作数是 " ^ type_of_vm_value v1 ^ " 和 " ^ type_of_vm_value v2 ^ "，需要布尔值")))
    | Not ->
        (match pop () with
         | VBool b -> push (VBool (not b))
         | v -> raise (VMError ("类型错误: not 的操作数是 " ^ type_of_vm_value v ^ "，需要布尔值")))

    (* 控制流 *)
    | Jump addr -> pc := addr
    | JumpIfFalse addr ->
        (match pop () with
         | VBool false -> pc := addr
         | VBool true -> ()
         | v -> raise (VMError ("类型错误: if 的条件是 " ^ type_of_vm_value v ^ "，需要布尔值")))

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
           | v -> raise (VMError ("类型错误: 调用需要函数，但得到 " ^ type_of_vm_value v)))
      | TailCall ->
          (match pop () with
           | VClosure (closure_env, param, func_code, _) ->
               let arg = pop () in
               (* 尾调用：复用当前栈帧，不保存调用者状态 *)
               pc := 0;
               stack := [];
               env := (param, arg) :: closure_env;
               execute_block func_code
           | v -> raise (VMError ("类型错误: 调用需要函数，但得到 " ^ type_of_vm_value v)))

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
         | v1, v2 -> raise (VMError ("类型错误: :: 的操作数是 " ^ type_of_vm_value v2 ^ " 和 " ^ type_of_vm_value v1 ^ "，需要值和列表")))
    | Head ->
        (match pop () with
         | VList (h :: _) -> push h
         | VList [] -> raise (VMError "head: 空列表")
         | _ -> raise (VMError "head: 需要列表"))
    | Tail ->
        (match pop () with
         | VList (_ :: t) -> push (VList t)
         | VList [] -> raise (VMError "tail: 空列表")
         | _ -> raise (VMError "tail: 需要列表"))
    | Length ->
        (match pop () with
         | VList l -> push (VInt (List.length l))
         | VString s -> push (VInt (String.length s))
         | _ -> raise (VMError "length: 需要列表或字符串"))
    | Index ->
        (match pop (), pop () with
         | VInt idx, VList vs ->
             if idx < 0 || idx >= List.length vs then
               raise (VMError ("索引越界: " ^ string_of_int idx))
             else push (List.nth vs idx)
         | VInt idx, VString s ->
             if idx < 0 || idx >= String.length s then
               raise (VMError ("字符串索引越界: " ^ string_of_int idx))
             else push (VString (String.make 1 s.[idx]))
         | v1, v2 -> raise (VMError ("类型错误: 索引的对象是 " ^ type_of_vm_value v1 ^ "，索引值是 " ^ type_of_vm_value v2 ^ "，需要列表/字符串和整数")))

    (* 字符串 *)
    | Concat ->
        (match pop (), pop () with
         | VString b, VString a -> push (VString (a ^ b))
         | v1, v2 -> raise (VMError ("类型错误: ^ 的操作数是 " ^ type_of_vm_value v1 ^ " 和 " ^ type_of_vm_value v2 ^ "，需要字符串")))

    (* ADT *)
    | PushCtor (name, arity) ->
        let rec loop acc n =
          if n = 0 then acc
          else loop (pop () :: acc) (n - 1)
        in
        push (VCtor (name, loop [] arity))
    | TestCtor name ->
        (match pop () with
         | VCtor (c, _) -> push (VBool (c = name))
         | v -> raise (VMError ("类型错误: TestCtor 需要构造函数，但得到 " ^ type_of_vm_value v)))
    | GetCtorArg idx ->
        (match pop () with
         | VCtor (_, args) when idx >= 0 && idx < List.length args ->
             push (List.nth args idx)
         | VCtor (_, args) ->
             raise (VMError ("构造函数参数索引越界: " ^ string_of_int idx ^ "，共有 " ^ string_of_int (List.length args) ^ " 个参数"))
         | v -> raise (VMError ("类型错误: GetCtorArg 需要构造函数，但得到 " ^ type_of_vm_value v)))

    (* 引用 *)
    | MakeRef ->
        let v = pop () in
        push (VRef (ref v))
    | Deref ->
        (match pop () with
         | VRef r -> push !r
         | v -> raise (VMError ("类型错误: 解引用需要 ref，但得到 " ^ type_of_vm_value v)))
    | SetRef ->
        (match pop (), pop () with
         | VRef r, v -> r := v; push VUnit
         | v, _ -> raise (VMError ("类型错误: 赋值需要 ref，但得到 " ^ type_of_vm_value v)))

    (* 其他 *)
    | Print ->
        let v = pop () in
        print_endline (string_of_vm_value v);
        push VUnit
    | Pop -> ignore (pop ())
    | Dup ->
        match !stack with
        | v :: _ -> push v
        | [] -> raise (VMError "栈下溢")

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
