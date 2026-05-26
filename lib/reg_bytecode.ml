(** 寄存器虚拟机

    基于寄存器的虚拟机，相比栈式 VM：
    - 减少内存访问（消除频繁的 push/pop）
    - 指令更紧凑
    - 更容易做 JIT 编译
    
    寄存器分配策略：
    - 每个局部变量固定映射到一个寄存器
    - 临时值使用额外的寄存器
    - 函数参数通过寄存器传递
*)

open Ast

(** 寄存器值 *)
type reg_value =
  | RVInt of int
  | RVBool of bool
  | RVUnit
  | RVNil
  | RVClosure of int * (string * reg_value) list  (* 函数索引 × 捕获环境 *)
  | RVString of string
  | RVRef of reg_value ref

let rec string_of_reg_value = function
  | RVInt n -> string_of_int n
  | RVBool b -> string_of_bool b
  | RVUnit -> "()"
  | RVNil -> "nil"
  | RVClosure (idx, _) -> Printf.sprintf "<closure %d>" idx
  | RVString s -> "\"" ^ s ^ "\""
  | RVRef r -> "ref " ^ string_of_reg_value !r

(** 寄存器指令 *)
type reg_instr =
  (* 常量加载 *)
  | RLoadConst of int * int         (* dest_reg, const_pool_idx *)
  | RLoadNil of int                 (* dest_reg *)
  
  (* 寄存器移动 *)
  | RMove of int * int              (* dest, src *)
  
  (* 算术运算：dest = src1 op src2 *)
  | RAdd of int * int * int
  | RSub of int * int * int
  | RMul of int * int * int
  | RDiv of int * int * int
  | RMod of int * int * int
  
  (* 比较：dest = src1 cmp src2 *)
  | REq of int * int * int
  | RNeq of int * int * int
  | RLt of int * int * int
  | RLe of int * int * int
  | RGt of int * int * int
  | RGe of int * int * int
  
  (* 逻辑 *)
  | RAnd of int * int * int
  | ROr of int * int * int
  | RNot of int * int               (* dest = not src *)
  
  (* 内存访问 *)
  | RLoadVar of int * string        (* dest_reg, var_name *)
  | RStoreVar of string * int       (* var_name, src_reg *)
  
  (* 控制流 *)
  | RJump of int                    (* offset *)
  | RJumpIfFalse of int * int       (* cond_reg, offset *)
  | RJumpIfTrue of int * int        (* cond_reg, offset *)
  
  (* 函数调用 *)
  | RLoadFunc of int * int          (* dest_reg, func_idx *)
  | RCall of int * int * int list   (* dest_reg, func_reg, arg_regs *)
  | RReturn of int                  (* return_reg *)
  | RTailCall of int * int list     (* func_reg, arg_regs *)
  
  (* 闭包 *)
  | RMakeClosure of int * int * (string * int) list  (* dest, func_idx, captured_vars *)
  
  (* 列表/元组 *)
  | RMakeList of int * int list     (* dest, elem_regs *)
  | RMakeTuple of int * int list    (* dest, elem_regs *)
  | RListGet of int * int * int     (* dest, list_reg, idx_reg *)
  | RListSet of int * int * int     (* list_reg, idx_reg, val_reg *)
  | RListLen of int * int           (* dest, list_reg *)
  
  (* 字符串 *)
  | RConcat of int * int * int      (* dest, s1_reg, s2_reg *)
  | RStringLen of int * int         (* dest, str_reg *)
  
  (* 引用 *)
  | RMakeRef of int * int           (* dest, src_reg *)
  | RDeref of int * int             (* dest, ref_reg *)
  | RAssignRef of int * int         (* ref_reg, val_reg *)
  
  (* 其他 *)
  | RPrint of int                   (* print_reg *)
  | RPop of int                     (* pop n elements *)
  | RNop                            (* no operation *)

(** 常量池 *)
type const_pool =
  | CPInt of int
  | CPBool of bool
  | CPString of string
  | CPUnit

(** 寄存器函数 *)
type reg_func = {
  name : string;
  params : string list;
  num_params : int;
  num_locals : int;
  max_regs : int;              (* 需要的最大寄存器数量 *)
  code : reg_instr array;
  constants : const_pool array;
}

(** 寄存器程序 *)
type reg_program = {
  functions : reg_func array;
  main_func : int;             (* main 函数索引 *)
  entry_point : int;           (* 入口函数（通常是 main） *)
}

(** 将常量转为字符串 *)
let string_of_const = function
  | CPInt n -> string_of_int n
  | CPBool b -> string_of_bool b
  | CPString s -> "\"" ^ s ^ "\""
  | CPUnit -> "()"

(** 将指令转为字符串（用于调试/反汇编） *)
let string_of_reg_instr = function
  | RLoadConst (d, c) -> Printf.sprintf "load_const r%d, const[%d]" d c
  | RLoadNil d -> Printf.sprintf "load_nil r%d" d
  | RMove (d, s) -> Printf.sprintf "move r%d, r%d" d s
  | RAdd (d, s1, s2) -> Printf.sprintf "add r%d, r%d, r%d" d s1 s2
  | RSub (d, s1, s2) -> Printf.sprintf "sub r%d, r%d, r%d" d s1 s2
  | RMul (d, s1, s2) -> Printf.sprintf "mul r%d, r%d, r%d" d s1 s2
  | RDiv (d, s1, s2) -> Printf.sprintf "div r%d, r%d, r%d" d s1 s2
  | RMod (d, s1, s2) -> Printf.sprintf "mod r%d, r%d, r%d" d s1 s2
  | REq (d, s1, s2) -> Printf.sprintf "eq r%d, r%d, r%d" d s1 s2
  | RNeq (d, s1, s2) -> Printf.sprintf "neq r%d, r%d, r%d" d s1 s2
  | RLt (d, s1, s2) -> Printf.sprintf "lt r%d, r%d, r%d" d s1 s2
  | RLe (d, s1, s2) -> Printf.sprintf "le r%d, r%d, r%d" d s1 s2
  | RGt (d, s1, s2) -> Printf.sprintf "gt r%d, r%d, r%d" d s1 s2
  | RGe (d, s1, s2) -> Printf.sprintf "ge r%d, r%d, r%d" d s1 s2
  | RAnd (d, s1, s2) -> Printf.sprintf "and r%d, r%d, r%d" d s1 s2
  | ROr (d, s1, s2) -> Printf.sprintf "or r%d, r%d, r%d" d s1 s2
  | RNot (d, s) -> Printf.sprintf "not r%d, r%d" d s
  | RLoadVar (d, name) -> Printf.sprintf "load_var r%d, %s" d name
  | RStoreVar (name, s) -> Printf.sprintf "store_var %s, r%d" name s
  | RJump offset -> Printf.sprintf "jump %d" offset
  | RJumpIfFalse (c, offset) -> Printf.sprintf "jump_if_false r%d, %d" c offset
  | RJumpIfTrue (c, offset) -> Printf.sprintf "jump_if_true r%d, %d" c offset
  | RLoadFunc (d, f) -> Printf.sprintf "load_func r%d, func[%d]" d f
  | RCall (d, f, args) -> Printf.sprintf "call r%d, r%d(%s)" d f (String.concat ", " (List.map (fun r -> "r" ^ string_of_int r) args))
  | RReturn r -> Printf.sprintf "return r%d" r
  | RTailCall (f, args) -> Printf.sprintf "tail_call r%d(%s)" f (String.concat ", " (List.map (fun r -> "r" ^ string_of_int r) args))
  | RMakeClosure (d, f, captures) -> Printf.sprintf "make_closure r%d, func[%d]" d f
  | RMakeList (d, elems) -> Printf.sprintf "make_list r%d, [%s]" d (String.concat ", " (List.map (fun r -> "r" ^ string_of_int r) elems))
  | RMakeTuple (d, elems) -> Printf.sprintf "make_tuple r%d, (%s)" d (String.concat ", " (List.map (fun r -> "r" ^ string_of_int r) elems))
  | RListGet (d, l, i) -> Printf.sprintf "list_get r%d, r%d[r%d]" d l i
  | RListSet (l, i, v) -> Printf.sprintf "list_set r%d[r%d], r%d" l i v
  | RListLen (d, l) -> Printf.sprintf "list_len r%d, r%d" d l
  | RConcat (d, s1, s2) -> Printf.sprintf "concat r%d, r%d, r%d" d s1 s2
  | RStringLen (d, s) -> Printf.sprintf "string_len r%d, r%d" d s
  | RMakeRef (d, s) -> Printf.sprintf "make_ref r%d, r%d" d s
  | RDeref (d, r) -> Printf.sprintf "deref r%d, r%d" d r
  | RAssignRef (r, v) -> Printf.sprintf "assign_ref r%d, r%d" r v
  | RPrint r -> Printf.sprintf "print r%d" r
  | RPop n -> Printf.sprintf "pop %d" n
  | RNop -> "nop"

(** 反汇编寄存器程序 *)
let disassemble prog =
  let buf = Buffer.create 1024 in
  Array.iteri (fun idx func ->
    Buffer.add_string buf (Printf.sprintf "\nFunction %d: %s (params=%d, locals=%d, max_regs=%d)\n"
      idx func.name func.num_params func.num_locals func.max_regs);
    
    (* 打印常量池 *)
    if Array.length func.constants > 0 then begin
      Buffer.add_string buf "Constants:\n";
      Array.iteri (fun i c ->
        Buffer.add_string buf (Printf.sprintf "  [%d] = %s\n" i (string_of_const c))) func.constants
    end;
    
    (* 打印指令 *)
    Array.iteri (fun i instr ->
      Buffer.add_string buf (Printf.sprintf "  %04d: %s\n" i (string_of_reg_instr instr))) func.code
  ) prog.functions;
  Buffer.contents buf
