(** JIT mmap 接口

    通过 C stub 分配 RWX（可读可写可执行）内存。
*)

(** 分配 RWX 内存，返回地址（nativeint） *)
external alloc : int -> Nativeint.t = "jit_mmap_alloc"

(** 释放 mmap 内存 *)
external free : Nativeint.t -> int -> unit = "jit_mmap_free"

(** 写入单个字节 *)
external write_byte : Nativeint.t -> int -> int -> unit = "jit_mmap_write_byte"

(** 写入整个字节数组 *)
external write_bytes : Nativeint.t -> bytes -> unit = "jit_mmap_write_bytes"

(** 执行机器码（无参数，返回 int） *)
external execute_int : Nativeint.t -> int = "jit_mmap_execute_int"

(** 将字节码写入可执行内存并执行 *)
let execute_code code size =
  let addr = alloc size in
  write_bytes addr code;
  let result = execute_int addr in
  free addr size;
  result
