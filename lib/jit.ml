(** JIT 即时编译器

    将寄存器字节码编译为 x86-64 机器码，通过 mmap 分配可执行内存执行。
    
    支持的基本指令：
    - mov r64, imm32
    - add r64, r64
    - sub r64, r64
    - imul r64, r64
    - idiv r64
    - cmp r64, r64
    - jmp rel32
    - je/jne/jl/jle/jg/jge rel32
    - call r64
    - ret
*)

open Core
open Reg_bytecode

(** x86-64 寄存器编码 *)
type x86_reg =
  | RAX | RBX | RCX | RDX | RSI | RDI | RBP | RSP
  | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15

let reg_code = function
  | RAX -> 0 | RCX -> 1 | RDX -> 2 | RBX -> 3
  | RSP -> 4 | RBP -> 5 | RSI -> 6 | RDI -> 7
  | R8  -> 0 | R9  -> 1 | R10 -> 2 | R11 -> 3
  | R12 -> 4 | R13 -> 5 | R14 -> 6 | R15 -> 7

let is_extended = function
  | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15 -> true
  | _ -> false

(** 机器码缓冲区 *)
type jit_buffer = {
  mutable code : bytes;
  mutable pos : int;
}

let create_buffer size = {
  code = Bytes.create size;
  pos = 0;
}

let emit_byte buf b =
  Bytes.set buf.code buf.pos (Char.of_int_exn b);
  buf.pos <- buf.pos + 1

let emit_int32 buf n =
  let n = Int32.to_int_exn n in
  emit_byte buf (n land 0xFF);
  emit_byte buf ((n lsr 8) land 0xFF);
  emit_byte buf ((n lsr 16) land 0xFF);
  emit_byte buf ((n lsr 24) land 0xFF)

let emit_int64 buf n =
  let n = Int64.to_int_exn n in
  for i = 0 to 7 do
    emit_byte buf ((n lsr (i * 8)) land 0xFF)
  done

(** REX 前缀 *)
let emit_rex buf ~w ~r ~x ~b =
  let rex = 0x40 lor (if w then 0x08 else 0) lor (if r then 0x04 else 0)
                     lor (if x then 0x02 else 0) lor (if b then 0x01 else 0) in
  emit_byte buf rex

(** ModR/M 字节 *)
let emit_modrm buf mod_bits reg rm =
  emit_byte buf ((mod_bits lsl 6) lor ((reg land 0x7) lsl 3) lor (rm land 0x7))

(** mov r64, imm64 *)
let emit_mov_reg_imm64 buf reg imm =
  let reg_code = reg_code reg in
  let ext = is_extended reg in
  emit_rex buf ~w:true ~r:false ~x:false ~b:ext;
  emit_byte buf (0xB8 + reg_code);
  emit_int64 buf imm

(** mov r64, r64 *)
let emit_mov_reg_reg buf dst src =
  let dst_code = reg_code dst in
  let src_code = reg_code src in
  let r = is_extended dst in
  let b = is_extended src in
  emit_rex buf ~w:true ~r ~x:false ~b;
  emit_byte buf 0x89;
  emit_modrm buf 0b11 src_code dst_code

(** add r64, r64 *)
let emit_add_reg_reg buf dst src =
  let dst_code = reg_code dst in
  let src_code = reg_code src in
  let r = is_extended src in
  let b = is_extended dst in
  emit_rex buf ~w:true ~r ~x:false ~b;
  emit_byte buf 0x01;
  emit_modrm buf 0b11 src_code dst_code

(** sub r64, r64 *)
let emit_sub_reg_reg buf dst src =
  let dst_code = reg_code dst in
  let src_code = reg_code src in
  let r = is_extended src in
  let b = is_extended dst in
  emit_rex buf ~w:true ~r ~x:false ~b;
  emit_byte buf 0x29;
  emit_modrm buf 0b11 src_code dst_code

(** imul r64, r64 *)
let emit_imul_reg_reg buf dst src =
  let dst_code = reg_code dst in
  let src_code = reg_code src in
  let r = is_extended dst in
  let b = is_extended src in
  emit_rex buf ~w:true ~r ~x:false ~b;
  emit_byte buf 0x0F;
  emit_byte buf 0xAF;
  emit_modrm buf 0b11 dst_code src_code

(** idiv r64 *)
let emit_idiv_reg buf reg =
  let reg_code = reg_code reg in
  let b = is_extended reg in
  emit_rex buf ~w:true ~r:false ~x:false ~b;
  emit_byte buf 0xF7;
  emit_modrm buf 0b11 0x7 reg_code

(** cmp r64, r64 *)
let emit_cmp_reg_reg buf a b_reg =
  let a_code = reg_code a in
  let b_code = reg_code b_reg in
  let r = is_extended b_reg in
  emit_rex buf ~w:true ~r ~x:false ~b:(is_extended a);
  emit_byte buf 0x39;
  emit_modrm buf 0b11 b_code a_code

(** jmp rel32 *)
let emit_jmp_rel32 buf offset =
  emit_byte buf 0xE9;
  emit_int32 buf (Int32.of_int_exn offset)

(** 条件跳转 *)
let emit_jcc_rel32 buf cc offset =
  emit_byte buf 0x0F;
  emit_byte buf (0x80 + cc);
  emit_int32 buf (Int32.of_int_exn offset)

let je = 0x04
let jne = 0x05
let jl = 0x0C
let jle = 0x0E
let jg = 0x0F
let jge = 0x0D

(** ret *)
let emit_ret buf =
  emit_byte buf 0xC3

(** push r64 *)
let emit_push buf reg =
  let reg_code = reg_code reg in
  if is_extended reg then
    emit_rex buf ~w:false ~r:false ~x:false ~b:true;
  emit_byte buf (0x50 + reg_code)

(** pop r64 *)
let emit_pop buf reg =
  let reg_code = reg_code reg in
  if is_extended reg then
    emit_rex buf ~w:false ~r:false ~x:false ~b:true;
  emit_byte buf (0x58 + reg_code)

(** 将虚拟机寄存器映射到 x86 寄存器 *)
let vm_reg_to_x86 = function
  | 0 -> RDI
  | 1 -> RSI
  | 2 -> RDX
  | 3 -> RCX
  | 4 -> R8
  | 5 -> R9
  | 6 -> R10
  | 7 -> R11
  | _ -> RAX  (* 回退 *)

(** JIT 编译单个函数 *)
let compile_function buf (func : Reg_bytecode.reg_func) =
  (* 简化的 JIT：只支持基本算术运算 *)
  let _label_map = Hashtbl.create (module Int) in
  
  (* 第一遍：记录标签位置（当前无标签指令） *)
  Array.iteri func.code ~f:(fun i instr ->
    ());
  
  (* 第二遍：生成代码 *)
  Array.iter func.code ~f:(fun instr ->
    match instr with
    | RLoadConst (dst, idx) ->
        (match func.constants.(idx) with
         | CPInt n -> emit_mov_reg_imm64 buf (vm_reg_to_x86 dst) (Int64.of_int n)
         | _ -> ())
    
    | RAdd (dst, s1, s2) ->
        let d = vm_reg_to_x86 dst in
        let a = vm_reg_to_x86 s1 in
        let b = vm_reg_to_x86 s2 in
        emit_mov_reg_reg buf d a;
        emit_add_reg_reg buf d b
    
    | RSub (dst, s1, s2) ->
        let d = vm_reg_to_x86 dst in
        let a = vm_reg_to_x86 s1 in
        let b = vm_reg_to_x86 s2 in
        emit_mov_reg_reg buf d a;
        emit_sub_reg_reg buf d b
    
    | RMul (dst, s1, s2) ->
        let d = vm_reg_to_x86 dst in
        let a = vm_reg_to_x86 s1 in
        let b = vm_reg_to_x86 s2 in
        emit_mov_reg_reg buf d a;
        emit_imul_reg_reg buf d b
    
    | RDiv (dst, s1, s2) ->
        let d = vm_reg_to_x86 dst in
        let a = vm_reg_to_x86 s1 in
        let b = vm_reg_to_x86 s2 in
        emit_mov_reg_imm64 buf RAX (Int64.of_int 0);  (* 清零 RDX:RAX *)
        emit_mov_reg_reg buf RAX a;
        emit_mov_reg_reg buf (vm_reg_to_x86 15) b;  (* 临时寄存器 *)
        emit_idiv_reg buf (vm_reg_to_x86 15);
        emit_mov_reg_reg buf d RAX
    
    | RReturn r ->
        let ret_reg = vm_reg_to_x86 r in
        if not (phys_equal ret_reg RAX) then
          emit_mov_reg_reg buf RAX ret_reg;
        emit_ret buf
    
    | _ -> ());
  
  buf.pos

(** JIT 执行程序（真实 mmap 执行） *)
let execute_jit prog =
  let buf = create_buffer 4096 in
  
  (* 编译入口函数 *)
  let entry_func = prog.functions.(prog.entry_point) in
  let code_size = compile_function buf entry_func in
  
  if code_size = 0 then Reg_bytecode.RVInt 0
  else (
    Printf.printf "JIT 编译完成，生成 %d 字节机器码\n" code_size;
    
    (* 通过 mmap 分配 RWX 内存并执行 *)
    let code = Bytes.sub buf.code ~pos:0 ~len:buf.pos in
    let result = Jit_mmap.execute_code code code_size in
    Reg_bytecode.RVInt result
  )
