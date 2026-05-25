(** WASM 二进制编码器

    将内部 WASM 指令表示编码为 WebAssembly 二进制格式 (.wasm)。
    
    WASM 二进制格式规范：
    - Magic: 0x00 0x61 0x73 0x6d
    - Version: 0x01 0x00 0x00 0x00
    - Sections with LEB128 encoding
*)

open Core

(** WASM 值类型 *)
type val_type = I32 | I64 | F32 | F64

(** WASM 二进制中的类型编码 *)
let encode_val_type = function
  | I32 -> 0x7f
  | I64 -> 0x7e
  | F32 -> 0x7d
  | F64 -> 0x7c

(** LEB128 无符号整数编码 *)
let rec encode_u32 n =
  if n < 0x80 then [Char.of_int_exn n]
  else
    let byte = Char.of_int_exn ((n land 0x7f) lor 0x80) in
    byte :: encode_u32 (n lsr 7)

(** LEB128 有符号整数编码 *)
let rec encode_s32 n =
  let more = ref true in
  let result = ref [] in
  let num = ref n in
  while !more do
    let byte = !num land 0x7f in
    num := !num asr 7;
    if (!num = 0 && (byte land 0x40) = 0) ||
       (!num = -1 && (byte land 0x40) <> 0) then
      more := false
    else
      num := byte lor 0x80;
    result := Char.of_int_exn byte :: !result
  done;
  List.rev !result

(** 字节缓冲区 *)
type buffer = {
  mutable data : char list;
}

let create_buffer () = { data = [] }

let push_char buf c = buf.data <- c :: buf.data
let push_bytes buf bytes = buf.data <- List.rev bytes @ buf.data

let push_u8 buf n = push_char buf (Char.of_int_exn n)
let push_u32 buf n = push_bytes buf (encode_u32 n)
let push_s32 buf n = push_bytes buf (encode_s32 n)

let push_string buf s =
  push_u32 buf (String.length s);
  String.iter s ~f:(push_char buf)

(** WASM section IDs *)
type section_id =
  | Custom of string
  | Type
  | Import
  | Function
  | Table
  | Memory
  | Global
  | Export
  | Start
  | Element
  | Code
  | Data
  | DataCount

let section_id_code = function
  | Custom _ -> 0
  | Type -> 1
  | Import -> 2
  | Function -> 3
  | Table -> 4
  | Memory -> 5
  | Global -> 6
  | Export -> 7
  | Start -> 8
  | Element -> 9
  | Code -> 10
  | Data -> 11
  | DataCount -> 12

(** WASM 操作码 *)
type opcode =
  | End
  | Return
  | Drop
  | Select
  | LocalGet of int
  | LocalSet of int
  | LocalTee of int
  | I32Const of int
  | I32Add
  | I32Sub
  | I32Mul
  | I32DivS
  | I32Eq
  | I32Ne
  | I32LtS
  | I32LeS
  | I32GtS
  | I32GeS
  | I32And
  | I32Or
  | I32Xor
  | I32Shl
  | I32ShrS
  | MemorySize
  | MemoryGrow
  | I32Load of int
  | I32Store of int
  | Nop
  | Block of val_type option * opcode list
  | Loop of val_type option * opcode list
  | If of val_type option * opcode list * opcode list
  | Br of int
  | BrIf of int
  | Call of int
  | CallIndirect of int

let rec encode_opcode buf = function
  | End -> push_u8 buf 0x0b
  | Return -> push_u8 buf 0x0f
  | Drop -> push_u8 buf 0x1a
  | Select -> push_u8 buf 0x1b
  | LocalGet i -> push_u8 buf 0x20; push_u32 buf i
  | LocalSet i -> push_u8 buf 0x21; push_u32 buf i
  | LocalTee i -> push_u8 buf 0x22; push_u32 buf i
  | I32Const n -> push_u8 buf 0x41; push_s32 buf n
  | I32Add -> push_u8 buf 0x6a
  | I32Sub -> push_u8 buf 0x6b
  | I32Mul -> push_u8 buf 0x6c
  | I32DivS -> push_u8 buf 0x6d
  | I32Eq -> push_u8 buf 0x46
  | I32Ne -> push_u8 buf 0x47
  | I32LtS -> push_u8 buf 0x48
  | I32LeS -> push_u8 buf 0x4c
  | I32GtS -> push_u8 buf 0x4a
  | I32GeS -> push_u8 buf 0x4e
  | I32And -> push_u8 buf 0x71
  | I32Or -> push_u8 buf 0x72
  | I32Xor -> push_u8 buf 0x73
  | I32Shl -> push_u8 buf 0x74
  | I32ShrS -> push_u8 buf 0x75
  | MemorySize -> push_u8 buf 0x3f; push_u8 buf 0x00
  | MemoryGrow -> push_u8 buf 0x40; push_u8 buf 0x00
  | I32Load align -> push_u8 buf 0x28; push_u32 buf align; push_u32 buf 0
  | I32Store align -> push_u8 buf 0x36; push_u32 buf align; push_u32 buf 0
  | Nop -> push_u8 buf 0x01
  | Block (result, ops) ->
      push_u8 buf 0x02;
      (match result with
       | Some t -> push_u8 buf (encode_val_type t)
       | None -> push_u8 buf 0x40);
      List.iter ops ~f:(encode_opcode buf);
      push_u8 buf 0x0b
  | Loop (result, ops) ->
      push_u8 buf 0x03;
      (match result with
       | Some t -> push_u8 buf (encode_val_type t)
       | None -> push_u8 buf 0x40);
      List.iter ops ~f:(encode_opcode buf);
      push_u8 buf 0x0b
  | If (result, then_ops, else_ops) ->
      push_u8 buf 0x04;
      (match result with
       | Some t -> push_u8 buf (encode_val_type t)
       | None -> push_u8 buf 0x40);
      List.iter then_ops ~f:(encode_opcode buf);
      if List.length else_ops > 0 then begin
        push_u8 buf 0x05;
        List.iter else_ops ~f:(encode_opcode buf)
      end;
      push_u8 buf 0x0b
  | Br i -> push_u8 buf 0x0c; push_u32 buf i
  | BrIf i -> push_u8 buf 0x0d; push_u32 buf i
  | Call i -> push_u8 buf 0x10; push_u32 buf i
  | CallIndirect i -> push_u8 buf 0x11; push_u32 buf i; push_u8 buf 0x00

(** WASM 函数 *)
type func = {
  locals : val_type list;
  body : opcode list;
}

(** WASM 模块 *)
type wasm_module = {
  types : (val_type list * val_type list) list;  (* params * results *)
  funcs : func list;
  exports : (string * int) list;  (* name * func_idx *)
  memories : int list;  (* initial pages *)
}

(** 编码类型段 *)
let encode_type_section buf types =
  let section_buf = create_buffer () in
  push_u32 section_buf (List.length types);
  List.iter types ~f:(fun (params, results) ->
    push_u8 section_buf 0x60;  (* func type *)
    push_u32 section_buf (List.length params);
    List.iter params ~f:(fun t -> push_u8 section_buf (encode_val_type t));
    push_u32 section_buf (List.length results);
    List.iter results ~f:(fun t -> push_u8 section_buf (encode_val_type t))
  );
  
  push_u8 buf (section_id_code Type);
  push_u32 buf (List.length section_buf.data);
  buf.data <- section_buf.data @ buf.data

(** 编码函数段 *)
let encode_func_section buf func_count =
  let section_buf = create_buffer () in
  push_u32 section_buf func_count;
  for i = 0 to func_count - 1 do
    push_u32 section_buf i  (* type index *)
  done;
  
  push_u8 buf (section_id_code Function);
  push_u32 buf (List.length section_buf.data);
  buf.data <- section_buf.data @ buf.data

(** 编码内存段 *)
let encode_memory_section buf memories =
  match memories with
  | [] -> ()
  | pages ->
      let section_buf = create_buffer () in
      push_u32 section_buf (List.length memories);
      List.iter pages ~f:(fun p ->
        push_u8 section_buf 0x00;  (* flags: no max *)
        push_u32 section_buf p)
      ;
      
      push_u8 buf (section_id_code Memory);
      push_u32 buf (List.length section_buf.data);
      buf.data <- section_buf.data @ buf.data

(** 编码导出段 *)
let encode_export_section buf exports =
  let section_buf = create_buffer () in
  push_u32 section_buf (List.length exports);
  List.iter exports ~f:(fun (name, idx) ->
    push_string section_buf name;
    push_u8 section_buf 0x00;  (* export kind: func *)
    push_u32 section_buf idx
  );
  
  push_u8 buf (section_id_code Export);
  push_u32 buf (List.length section_buf.data);
  buf.data <- section_buf.data @ buf.data

(** 编码代码段 *)
let encode_code_section buf funcs =
  let section_buf = create_buffer () in
  push_u32 section_buf (List.length funcs);
  List.iter funcs ~f:(fun func ->
    let func_buf = create_buffer () in
    (* locals: count groups of same type *)
    push_u32 func_buf 0;  (* simplified: no locals for now *)
    (* body *)
    List.iter func.body ~f:(encode_opcode func_buf);
    push_u8 func_buf 0x0b;  (* end *)
    
    push_u32 section_buf (List.length func_buf.data);
    buf.data <- func_buf.data @ buf.data
  );
  
  push_u8 buf (section_id_code Code);
  push_u32 buf (List.length section_buf.data);
  buf.data <- section_buf.data @ buf.data

(** 编码完整模块 *)
let encode_module m =
  let buf = create_buffer () in
  
  (* Magic *)
  push_char buf '\x00';
  push_char buf 'a';
  push_char buf 's';
  push_char buf 'm';
  
  (* Version *)
  push_u8 buf 1;
  push_u8 buf 0;
  push_u8 buf 0;
  push_u8 buf 0;
  
  (* Type section *)
  if List.length m.types > 0 then encode_type_section buf m.types;
  
  (* Function section *)
  if List.length m.funcs > 0 then encode_func_section buf (List.length m.funcs);
  
  (* Memory section *)
  if List.length m.memories > 0 then encode_memory_section buf m.memories;
  
  (* Export section *)
  if List.length m.exports > 0 then encode_export_section buf m.exports;
  
  (* Code section *)
  if List.length m.funcs > 0 then encode_code_section buf m.funcs;
  
  String.of_char_list (List.rev buf.data)

(** 从 WAT 字符串提取简单模块（简化版） *)
let parse_simple_wat wat_code =
  (* 这是一个简化的解析器，仅用于演示 *)
  (* 实际应该解析完整的 WAT 语法 *)
  let func = {
    locals = [];
    body = [
      I32Const 42;
      Return;
    ];
  } in
  {
    types = [([I32], [I32])];
    funcs = [func];
    exports = [("main", 0)];
    memories = [1];
  }

(** 编译并输出 WASM 二进制 *)
let compile_to_wasm_binary wat_code =
  let module_ = parse_simple_wat wat_code in
  encode_module module_
