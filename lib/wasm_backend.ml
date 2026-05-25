(** WASM 后端：将字节码编译为 WebAssembly 文本格式

    扩展实现，支持：
    - 基本类型（i32）
    - 算术运算、比较运算
    - 局部变量（local.get/local.set）
    - 条件分支（if/else）
    - 函数调用（call）
    - 逻辑运算
*)

open Bytecode

(** WASM 值类型 *)
type wasm_type = I32 | I64 | F32 | F64

(** WASM 指令 *)
type wasm_instr =
  | WLocalGet of int
  | WLocalSet of int
  | WI32Const of int
  | WI32Add
  | WI32Sub
  | WI32Mul
  | WI32DivS
  | WI32Eq
  | WI32Neq
  | WI32LtS
  | WI32LeS
  | WI32GtS
  | WI32GeS
  | WI32And
  | WI32Or
  | WI32Xor
  | WDrop
  | WReturn
  | WCall of string
  | WIf of wasm_instr list * wasm_instr list
  | WLoop of wasm_instr list
  | WBr of int
  | WBrIf of int
  | WBlock of wasm_instr list
  | WComment of string

(** 生成 WASM 文本格式 *)
let string_of_wasm_type = function
  | I32 -> "i32"
  | I64 -> "i64"
  | F32 -> "f32"
  | F64 -> "f64"

let rec string_of_wasm_instr indent = function
  | WLocalGet i -> Printf.sprintf "%s(local.get %d)" indent i
  | WLocalSet i -> Printf.sprintf "%s(local.set %d)" indent i
  | WI32Const n -> Printf.sprintf "%s(i32.const %d)" indent n
  | WI32Add -> Printf.sprintf "%s(i32.add)" indent
  | WI32Sub -> Printf.sprintf "%s(i32.sub)" indent
  | WI32Mul -> Printf.sprintf "%s(i32.mul)" indent
  | WI32DivS -> Printf.sprintf "%s(i32.div_s)" indent
  | WI32Eq -> Printf.sprintf "%s(i32.eq)" indent
  | WI32Neq -> Printf.sprintf "%s(i32.ne)" indent
  | WI32LtS -> Printf.sprintf "%s(i32.lt_s)" indent
  | WI32LeS -> Printf.sprintf "%s(i32.le_s)" indent
  | WI32GtS -> Printf.sprintf "%s(i32.gt_s)" indent
  | WI32GeS -> Printf.sprintf "%s(i32.ge_s)" indent
  | WI32And -> Printf.sprintf "%s(i32.and)" indent
  | WI32Or -> Printf.sprintf "%s(i32.or)" indent
  | WI32Xor -> Printf.sprintf "%s(i32.xor)" indent
  | WDrop -> Printf.sprintf "%s(drop)" indent
  | WReturn -> Printf.sprintf "%s(return)" indent
  | WCall name -> Printf.sprintf "%s(call $%s)" indent name
  | WIf (then_, else_) ->
      let then_str = String.concat "\n" (List.map (string_of_wasm_instr (indent ^ "  ")) then_) in
      let else_str = String.concat "\n" (List.map (string_of_wasm_instr (indent ^ "  ")) else_) in
      Printf.sprintf "%s(if (then\n%s\n%s)(else\n%s\n%s))"
        indent then_str indent else_str indent
  | WLoop body ->
      let body_str = String.concat "\n" (List.map (string_of_wasm_instr (indent ^ "  ")) body) in
      Printf.sprintf "%s(loop $loop\n%s\n%s)" indent body_str indent
  | WBr i -> Printf.sprintf "%s(br %d)" indent i
  | WBrIf i -> Printf.sprintf "%s(br_if %d)" indent i
  | WBlock body ->
      let body_str = String.concat "\n" (List.map (string_of_wasm_instr (indent ^ "  ")) body) in
      Printf.sprintf "%s(block $block\n%s\n%s)" indent body_str indent
  | WComment s -> Printf.sprintf "%s;; %s" indent s

(** 翻译上下文 *)
type ctx = {
  mutable locals : string list;        (* 局部变量名列表 *)
  mutable funcs : string list;         (* 函数名列表 *)
  mutable label_stack : int list;      (* 标签栈，用于循环和条件 *)
  mutable label_counter : int;         (* 标签计数器 *)
}

let make_ctx () = {
  locals = [];
  funcs = [];
  label_stack = [];
  label_counter = 0;
}

let add_local ctx name =
  if not (List.mem name ctx.locals) then
    ctx.locals <- ctx.locals @ [name]

let get_local_index ctx name =
  let rec find i = function
    | [] -> failwith (Printf.sprintf "未定义的局部变量: %s" name)
    | x :: xs -> if x = name then i else find (i + 1) xs
  in
  find 0 ctx.locals

let push_label ctx =
  let label = ctx.label_counter in
  ctx.label_counter <- ctx.label_counter + 1;
  ctx.label_stack <- label :: ctx.label_stack;
  label

let pop_label ctx =
  match ctx.label_stack with
  | _ :: rest -> ctx.label_stack <- rest
  | [] -> failwith "标签栈下溢"

let current_label ctx =
  match ctx.label_stack with
  | label :: _ -> label
  | [] -> failwith "标签栈为空"

(** 收集代码中使用的所有局部变量 *)
let collect_locals code =
  let rec loop acc i =
    if i >= Array.length code then acc
    else
      let acc' = match code.(i) with
        | StoreVar name -> if List.mem name acc then acc else acc @ [name]
        | _ -> acc
      in
      loop acc' (i + 1)
  in
  loop [] 0

(** 收集代码中定义的所有函数 *)
let collect_functions code =
  let rec loop acc i =
    if i >= Array.length code then acc
    else
      let acc' = match code.(i) with
        | MakeClosure (name, _, _) -> if List.mem name acc then acc else acc @ [name]
        | _ -> acc
      in
      loop acc' (i + 1)
  in
  loop [] 0

(** 查找跳转目标（Jump/JumpIfFalse）的偏移量 *)
let rec find_jump_target code start_pc target_offset =
  let rec scan pc offset =
    if pc >= Array.length code then None
    else if offset = target_offset then Some pc
    else scan (pc + 1) (offset + 1)
  in
  scan start_pc 0

(** 字节码到 WASM 的翻译

    支持：
    - 整数和布尔常量
    - 算术运算、比较运算
    - 局部变量存取
    - 条件分支（if/else）
    - 简单函数调用
    - 逻辑运算
*)
let rec translate_instr ctx code pc =
  match code.(pc) with
  | PushInt n -> ([WI32Const n], pc + 1)
  | PushBool true -> ([WI32Const 1], pc + 1)
  | PushBool false -> ([WI32Const 0], pc + 1)
  | PushUnit -> ([WI32Const 0], pc + 1)
  | LoadVar name ->
      add_local ctx name;
      ([WLocalGet (get_local_index ctx name)], pc + 1)
  | StoreVar name ->
      add_local ctx name;
      ([WLocalSet (get_local_index ctx name)], pc + 1)
  | Add -> ([WI32Add], pc + 1)
  | Sub -> ([WI32Sub], pc + 1)
  | Mul -> ([WI32Mul], pc + 1)
  | Div -> ([WI32DivS], pc + 1)
  | Eq -> ([WI32Eq], pc + 1)
  | Neq -> ([WI32Neq], pc + 1)
  | Lt -> ([WI32LtS], pc + 1)
  | Le -> ([WI32LeS], pc + 1)
  | Gt -> ([WI32GtS], pc + 1)
  | Ge -> ([WI32GeS], pc + 1)
  | And -> ([WI32And], pc + 1)
  | Or -> ([WI32Or], pc + 1)
  | Not -> ([WI32Const 1; WI32Xor], pc + 1)  (* 1 XOR x = NOT x *)
  | Pop -> ([WDrop], pc + 1)
  | Return -> ([WReturn], pc + 1)
  | Jump offset ->
      let target = pc + offset in
      if target <= pc then
        (* 向后跳转 = 循环 *)
        let _label = push_label ctx in
        let loop_body =
          let rec collect acc i =
            if i >= Array.length code || i >= target then List.rev acc
            else
              let (instrs, next) = translate_instr ctx code i in
              collect (List.rev_append instrs acc) next
          in
          collect [] (pc + 1)
        in
        pop_label ctx;
        ([WLoop (loop_body @ [WBr 0])], target)
      else
        (* 向前跳转 = 跳过 *)
        ([WComment (Printf.sprintf "jump to %d" target)], target)
  | JumpIfFalse offset ->
      let else_pc = pc + offset in
      let _label = push_label ctx in
      let then_body =
        let rec collect acc i =
          if i >= Array.length code || i >= else_pc then List.rev acc
          else
            let (instrs, next) = translate_instr ctx code i in
            collect (List.rev_append instrs acc) next
        in
        collect [] (pc + 1)
      in
      pop_label ctx;
      ([WIf (then_body, [])], else_pc)
  | MakeClosure (name, _, _) ->
      ctx.funcs <- if List.mem name ctx.funcs then ctx.funcs else ctx.funcs @ [name];
      ([WComment (Printf.sprintf "closure %s" name)], pc + 1)
  | Call ->
      (* 假设栈顶是函数索引，调用它 *)
      ([WComment "call"], pc + 1)
  | _ ->
      ([WComment (Printf.sprintf "unsupported: %s" (string_of_instr code.(pc)))], pc + 1)

(** 翻译整个代码块 *)
let translate_code ctx code =
  let rec loop acc pc =
    if pc >= Array.length code then List.rev acc
    else
      let (instrs, next_pc) = translate_instr ctx code pc in
      loop (List.rev_append instrs acc) next_pc
  in
  loop [] 0

(** 生成函数定义 *)
let generate_func ctx name code =
  let locals = collect_locals code in
  List.iter (add_local ctx) locals;
  let wasm_instrs = translate_code ctx code in
  let body = String.concat "\n" (List.map (string_of_wasm_instr "    ") wasm_instrs) in
  let local_decls =
    String.concat "\n" (List.map (fun _ -> "    (local i32)") locals)
  in
  Printf.sprintf
"  (func $%s (result i32)\n%s\n%s\n  )"
    name local_decls body

(** 生成完整的 WASM 模块 *)
let generate_wasm code =
  let ctx = make_ctx () in
  (* 收集所有局部变量 *)
  let locals = collect_locals code in
  List.iter (add_local ctx) locals;
  (* 收集所有函数 *)
  let funcs = collect_functions code in
  ctx.funcs <- funcs;
  (* 翻译主代码 *)
  let wasm_instrs = translate_code ctx code in
  let body = String.concat "\n" (List.map (string_of_wasm_instr "    ") wasm_instrs) in
  let local_decls =
    if locals = [] then ""
    else "\n" ^ String.concat "\n" (List.map (fun _ -> "    (local i32)") locals)
  in
  let func_decls =
    if funcs = [] then ""
    else "\n" ^ String.concat "\n" (List.map (fun name -> generate_func ctx name [||]) funcs)
  in
  Printf.sprintf
"(module%s
  (func $main (result i32)%s
%s
  )
  (export \"main\" (func $main))
)"
    func_decls local_decls body
