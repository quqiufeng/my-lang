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
  | VTuple of vm_value list
  | VClosure of (string * vm_value) list * string * instr array * string option
    (** 闭包 = 捕获环境 × 参数名 × 函数代码 × 递归自引用名 *)
  | VCtor of string * vm_value list
    (** 代数数据类型构造函数 = 名称 × 参数列表 *)
  | VRef of vm_value ref
    (** 引用值 *)
  | VArray of vm_value array
    (** 数组值 *)
  | VRecord of (string * vm_value ref) list
    (** 记录值 *)

(** 获取 VM 值的类型描述（用于错误报告） *)
let rec type_of_vm_value = function
  | VInt _ -> "int"
  | VBool _ -> "bool"
  | VChar _ -> "char"
  | VString _ -> "string"
  | VUnit -> "unit"
  | VNil -> "nil"
  | VList _ -> "list"
  | VTuple _ -> "tuple"
  | VClosure _ -> "function"
  | VCtor (name, _) -> name
  | VRef _ -> "ref"
  | VArray _ -> "array"
  | VRecord _ -> "record"

let rec string_of_vm_value = function
  | VInt n -> string_of_int n
  | VBool true -> "true"
  | VBool false -> "false"
  | VChar c -> "'" ^ String.make 1 c ^ "'"
  | VString s -> "\"" ^ s ^ "\""
  | VUnit -> "()"
  | VNil -> "[]"
  | VList vs -> "[" ^ String.concat "; " (List.map string_of_vm_value vs) ^ "]"
  | VTuple vs -> "(" ^ String.concat ", " (List.map string_of_vm_value vs) ^ ")"
  | VClosure _ -> "<closure>"
  | VCtor (name, []) -> name
  | VCtor (name, [v]) -> name ^ " " ^ string_of_vm_value v
  | VCtor (name, vs) -> name ^ " (" ^ String.concat ", " (List.map string_of_vm_value vs) ^ ")"
  | VRef r -> "ref " ^ string_of_vm_value !r
  | VArray arr -> "[|" ^ String.concat "; " (List.map string_of_vm_value (Array.to_list arr)) ^ "|]"
  | VRecord fields -> "{" ^ String.concat "; " (List.map (fun (f, r) -> f ^ " = " ^ string_of_vm_value !r) fields) ^ "}"

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
  let stack_size = ref 1024 in
  let stack = Array.make !stack_size VUnit in
  let sp = ref 0 in                        (* 栈指针 *)
  let env = ref [] in                      (* 当前环境（变量绑定列表） *)
  let pc = ref 0 in                        (* 程序计数器 *)
  let call_stack = ref [] in               (* 调用栈：保存 (返回地址, 调用者栈指针, 调用者环境) *)
  let handler_stack = ref [] in            (* 异常处理栈：保存 (handler地址, handler栈指针, handler环境) *)

  (** 栈操作辅助函数 *)
  let push v =
    if !sp >= !stack_size then (
      let new_size = !stack_size * 2 in
      let new_stack = Array.make new_size VUnit in
      Array.blit stack 0 new_stack 0 !stack_size;
      stack_size := new_size;
      Array.set new_stack !sp v;
      sp := !sp + 1
    ) else (
      Array.set stack !sp v;
      sp := !sp + 1
    )
  in
  let pop () =
    if !sp <= 0 then raise (VMError "栈下溢");
    sp := !sp - 1;
    Array.get stack !sp
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
    (* 调试：打印指令和栈状态 *)
    (* Printf.printf "DEBUG [%04d] %s | stack: %s\n%!" (!pc - 1) (string_of_instr instr)
      (String.concat ", " (List.map string_of_vm_value (List.rev !stack))); *)
    match instr with
    | PushInt n -> push (VInt n)
    | PushBool b -> push (VBool b)
    | PushChar c -> push (VChar c)
    | PushString s -> push (VString s)
    | PushUnit -> push VUnit
    | PushNil -> push VNil

    | LoadVar x ->
        (try push (lookup !env x)
         with VMError msg ->
           raise (VMError msg))
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
             let cl = VClosure (!env, param, func_code, self_name) in
             push cl)

    (* 函数调用 *)
       | Call ->
           (let f = pop () in
            let arg = pop () in
            match f with
            | VClosure (closure_env, param, func_code, _) ->
                 (* 帧指针方案：保存调用者的 pc、sp、env，不复制栈 *)
                 call_stack := (!pc, !sp, !env) :: !call_stack;
                 pc := 0;
                 env := (param, arg) :: closure_env;
                 execute_block func_code
            | v -> raise (VMError ("类型错误: 调用需要函数，但得到 " ^ type_of_vm_value v)))
       | TailCall ->
           (match pop () with
            | VClosure (closure_env, param, func_code, _) ->
                let arg = pop () in
                (* 尾调用：复用当前栈帧，恢复 sp 到当前函数的栈底 *)
                (match !call_stack with
                 | (_, saved_sp, _) :: _ -> sp := saved_sp
                 | [] -> ());
                pc := 0;
                env := (param, arg) :: closure_env;
                execute_block func_code
            | v -> raise (VMError ("类型错误: 调用需要函数，但得到 " ^ type_of_vm_value v)))

    (* 函数返回 *)
    | Return ->
        let result = if !sp > 0 then Array.get stack (!sp - 1) else VUnit in
        (match !call_stack with
         | (old_pc, old_sp, old_env) :: rest ->
             (* 恢复调用者状态：pc、sp、env，将返回值压回调用者栈 *)
             pc := old_pc;
             sp := old_sp;
             env := old_env;
             call_stack := rest;
             push result
         | [] -> ());
        (* 总是抛出 ReturnExn 让 execute_block 退出 *)
        raise ReturnExn

    (* 列表操作 *)
    | MakeList n ->
        let rec loop acc n =
          if n = 0 then acc
          else loop (pop () :: acc) (n - 1)
        in
        push (VList (loop [] n))
    | MakeTuple n ->
        let rec loop acc n =
          if n = 0 then acc
          else loop (pop () :: acc) (n - 1)
        in
        push (VTuple (loop [] n))
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
         | VTuple vs -> push (VInt (List.length vs))
         | VString s -> push (VInt (String.length s))
         | _ -> raise (VMError "length: 需要列表或字符串"))
    | Index ->
        (match pop (), pop () with
         | VInt idx, VList vs ->
             if idx < 0 || idx >= List.length vs then
               raise (VMError ("索引越界: " ^ string_of_int idx))
             else push (List.nth vs idx)
         | VInt idx, VTuple vs ->
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
        let r = VRef (ref v) in
        
        push r
    | Deref ->
        (match pop () with
         | VRef r -> push !r
         | v -> raise (VMError ("类型错误: 解引用需要 ref，但得到 " ^ type_of_vm_value v)))
    | SetRef ->
        (match pop (), pop () with
         | v, VRef r -> r := v; push VUnit
         | _, v -> raise (VMError ("类型错误: 赋值需要 ref，但得到 " ^ type_of_vm_value v)))

    (* 数组 *)
    | MakeArray n ->
        let rec loop acc n =
          if n = 0 then acc
          else loop (pop () :: acc) (n - 1)
        in
        let arr = VArray (Array.of_list (loop [] n)) in
        
        push arr
    | ArrayGet ->
        (match pop (), pop () with
         | VInt idx, VArray arr ->
             if idx >= 0 && idx < Array.length arr then
               push (Array.get arr idx)
             else
               raise (VMError ("数组索引越界: " ^ string_of_int idx))
         | v1, v2 ->
             raise (VMError ("类型错误: ArrayGet 需要 int 和 array，但得到 " ^ type_of_vm_value v1 ^ " 和 " ^ type_of_vm_value v2)))
    | ArraySet ->
        (match pop (), pop (), pop () with
         | v, VInt idx, VArray arr ->
             if idx >= 0 && idx < Array.length arr then
               (Array.set arr idx v; push VUnit)
             else
               raise (VMError ("数组索引越界: " ^ string_of_int idx))
         | v1, v2, v3 ->
             raise (VMError ("类型错误: ArraySet 需要 array, int, value")))

    (* 记录 *)
    | MakeRecord n ->
        let rec loop acc n =
          if n = 0 then acc
          else
            let value = pop () in
            let key = pop () in
            match key with
            | VString k -> loop ((k, ref value) :: acc) (n - 1)
            | _ -> raise (VMError ("类型错误: MakeRecord 需要字符串键"))
        in
        let record = VRecord (loop [] n) in
        
        push record
    | RecordGet field ->
        (match pop () with
         | VRecord fields ->
             (match List.assoc_opt field fields with
              | Some r -> push !r
              | None -> raise (VMError ("记录没有字段: " ^ field)))
         | v -> raise (VMError ("类型错误: RecordGet 需要 record，但得到 " ^ type_of_vm_value v)))
    | RecordSet field ->
        (match pop (), pop () with
         | v, VRecord fields ->
             (match List.assoc_opt field fields with
              | Some r -> r := v; push VUnit
              | None -> push (VRecord (fields @ [(field, ref v)])))
         | v1, v2 ->
             raise (VMError ("类型错误: RecordSet 需要 record 和 value")))

    | CopyRecord ->
        (match pop () with
         | VRecord fields ->
             let copied = List.map (fun (k, r) -> (k, ref !r)) fields in
             push (VRecord copied)
         | v ->
             raise (VMError ("类型错误: CopyRecord 需要 record，但得到 " ^ type_of_vm_value v)))

    | Slice ->
        (match pop (), pop (), pop () with
         | VInt end_idx, VInt start_idx, VList vs ->
             let len = List.length vs in
             let real_start = min start_idx len in
             let real_end = if end_idx = -1 then len else min end_idx len in
             if real_start > real_end then push (VList [])
             else
               let rec take n = function
                 | [] -> []
                 | h :: t -> if n = 0 then [] else h :: take (n - 1) t
               in
               let rec drop n = function
                 | [] -> []
                 | h :: t -> if n = 0 then h :: t else drop (n - 1) t
               in
               push (VList (take (real_end - real_start) (drop real_start vs)))
         | VInt end_idx, VInt start_idx, VString s ->
             let len = String.length s in
             let real_start = min start_idx len in
             let real_end = if end_idx = -1 then len else min end_idx len in
             if real_start > real_end then push (VString "")
             else push (VString (String.sub s real_start (real_end - real_start)))
          | v1, v2, v3 ->
              raise (VMError ("类型错误: Slice 需要 int, int, list/string")))

    | MakeRange ->
        (match pop (), pop () with
         | VInt e, VInt s when s <= e ->
             push (VList (List.init (e - s + 1) (fun i -> VInt (s + i))))
         | VInt _, VInt _ ->
             push (VList [])
         | v1, v2 ->
             raise (VMError ("类型错误: 范围表达式需要 int 和 int，但得到 " ^ type_of_vm_value v1 ^ " 和 " ^ type_of_vm_value v2)))

    (* 其他 *)
    | Print ->
        let v = pop () in
        print_endline (string_of_vm_value v);
        push VUnit
    | Pop -> ignore (pop ())
    | Dup ->
        if !sp <= 0 then raise (VMError "栈下溢");
        push (Array.get stack (!sp - 1))

    | Nop -> ()

    (* 异常处理 *)
    | PushHandler addr ->
        (* 帧指针方案：保存当前 sp 和 env，不复制栈 *)
        handler_stack := (addr, !sp, !env) :: !handler_stack
    | PopHandler ->
        (match !handler_stack with
         | _ :: rest -> handler_stack := rest
         | [] -> raise (VMError "PopHandler: 异常处理栈为空"))
    | RaiseExn ->
        let exn_val = pop () in
        (match !handler_stack with
         | (addr, saved_sp, h_env) :: rest ->
             (* 恢复 handler 的状态：sp、env，将异常值压入栈，跳转到 handler *)
             pc := addr;
             sp := saved_sp;
             env := h_env;
             handler_stack := rest;
             push exn_val
         | [] ->
             raise (VMError ("未捕获异常: " ^ string_of_vm_value exn_val)))

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
  if !sp = 1 then Array.get stack 0
  else if !sp = 0 then VUnit
  else Array.get stack (!sp - 1)
