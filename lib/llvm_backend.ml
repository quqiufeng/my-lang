(** LLVM IR 代码生成器

    将寄存器字节码编译为 LLVM IR 文本。
    
    支持的指令：
    - 常量加载（整数、布尔值、字符串）
    - 算术运算（+ - * /）
    - 比较运算（= < <= > >=）
    - 逻辑运算（&& || not）
    - 控制流（jump, jump_if_false, jump_if_true）
    - 函数调用（call, return）
    - 变量加载/存储
*)

open Reg_bytecode

exception LLVMError of string

type llvm_ctx = {
    mutable label_counter : int;
    mutable var_counter : int;
}

let create_ctx () = { label_counter = 0; var_counter = 0 }

let fresh_label ctx =
    let n = ctx.label_counter in
    ctx.label_counter <- n + 1;
    Printf.sprintf "L%d" n

let fresh_var ctx =
    let n = ctx.var_counter in
    ctx.var_counter <- n + 1;
    Printf.sprintf "%%v%d" n

let llvm_type_of_const = function
    | CPInt _ -> "i64"
    | CPBool _ -> "i1"
    | CPString _ -> "i8*"
    | CPUnit -> "i64"

let llvm_const_value = function
    | CPInt n -> Printf.sprintf "%d" n
    | CPBool b -> if b then "1" else "0"
    | CPString s -> Printf.sprintf "getelementptr inbounds ([%d x i8], [%d x i8]* @str_%s, i64 0, i64 0)" (String.length s + 1) (String.length s + 1) (string_of_int (Hashtbl.hash s))
    | CPUnit -> "0"

let sanitize_name name =
    let buf = Buffer.create (String.length name) in
    for i = 0 to String.length name - 1 do
        let c = name.[i] in
        match c with
        | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '.' | '-' -> Buffer.add_char buf c
        | _ -> Buffer.add_char buf '_'
    done;
    Buffer.contents buf

(** 生成函数的 LLVM IR *)
let generate_function ctx func func_idx =
    let buf = Buffer.create 4096 in
    let emit line = Buffer.add_string buf (line ^ "\n") in
    
    let func_name = if func_idx = 0 then "main" else sanitize_name func.name in
    
    (* 函数头 *)
    let param_types =
        List.init func.num_params (fun _ -> "i64")
        |> String.concat ", "
    in
    emit (Printf.sprintf "define i64 @%s(%s) {" func_name param_types);
    
    (* 入口基本块 *)
    emit "entry:";
    
    (* 分配局部变量（alloca） *)
    for i = 0 to func.max_regs - 1 do
        emit (Printf.sprintf "  %%r%d = alloca i64" i)
    done;
    
    (* 初始化参数到寄存器 *)
    List.iteri (fun i param_name ->
        emit (Printf.sprintf "  store i64 %%%s, i64* %%r%d" param_name i)
    ) func.params;
    
    (* 当前活跃寄存器的 SSA 值映射：reg -> llvm_value *)
    let reg_values = Array.create func.max_regs (Printf.sprintf "%%r0_val") in
    
    (* 基本块标签 *)
    let block_labels = Array.create (Array.length func.code) "" in
    for i = 0 to Array.length func.code - 1 do
        block_labels.(i) <- Printf.sprintf "block_%d_%d" func_idx i
    done;
    
    (* 扫描跳转目标，标记需要标签的位置 *)
    let need_label = Array.create (Array.length func.code) false in
    need_label.(0) <- true;
    Array.iteri (fun pc instr ->
        match instr with
        | RJump offset -> need_label.(pc + offset) <- true
        | RJumpIfFalse (_, offset) | RJumpIfTrue (_, offset) ->
            need_label.(pc + offset) <- true;
            if pc + 1 < Array.length func.code then need_label.(pc + 1) <- true
        | _ -> ()
    ) func.code;
    
    (* 生成指令 *)
    let rec gen_instrs pc =
        if pc >= Array.length func.code then ()
        else begin
            if need_label.(pc) && pc > 0 then
                emit (Printf.sprintf "%s:" block_labels.(pc));
            
            let instr = func.code.(pc) in
            
            (match instr with
            | RLoadConst (d, c) ->
                let v = fresh_var ctx in
                let const_val = llvm_const_value func.constants.(c) in
                let ty = llvm_type_of_const func.constants.(c) in
                if ty = "i8*" then
                    emit (Printf.sprintf "  %s = bitcast i8* %s to i64" v const_val)
                else
                    emit (Printf.sprintf "  %s = add i64 0, %s" v const_val);
                reg_values.(d) <- v
            
            | RLoadNil d ->
                let v = fresh_var ctx in
                emit (Printf.sprintf "  %s = add i64 0, 0" v);
                reg_values.(d) <- v
            
            | RMove (d, s) ->
                reg_values.(d) <- reg_values.(s)
            
            | RAdd (d, s1, s2) ->
                let v = fresh_var ctx in
                emit (Printf.sprintf "  %s = add i64 %s, %s" v reg_values.(s1) reg_values.(s2));
                reg_values.(d) <- v
            
            | RSub (d, s1, s2) ->
                let v = fresh_var ctx in
                emit (Printf.sprintf "  %s = sub i64 %s, %s" v reg_values.(s1) reg_values.(s2));
                reg_values.(d) <- v
            
            | RMul (d, s1, s2) ->
                let v = fresh_var ctx in
                emit (Printf.sprintf "  %s = mul i64 %s, %s" v reg_values.(s1) reg_values.(s2));
                reg_values.(d) <- v
            
            | RDiv (d, s1, s2) ->
                let v = fresh_var ctx in
                emit (Printf.sprintf "  %s = sdiv i64 %s, %s" v reg_values.(s1) reg_values.(s2));
                reg_values.(d) <- v
            
            | RMod (d, s1, s2) ->
                let v = fresh_var ctx in
                emit (Printf.sprintf "  %s = srem i64 %s, %s" v reg_values.(s1) reg_values.(s2));
                reg_values.(d) <- v
            
            | REq (d, s1, s2) ->
                let cmp = fresh_var ctx in
                let v = fresh_var ctx in
                emit (Printf.sprintf "  %s = icmp eq i64 %s, %s" cmp reg_values.(s1) reg_values.(s2));
                emit (Printf.sprintf "  %s = zext i1 %s to i64" v cmp);
                reg_values.(d) <- v
            
            | RNeq (d, s1, s2) ->
                let cmp = fresh_var ctx in
                let v = fresh_var ctx in
                emit (Printf.sprintf "  %s = icmp ne i64 %s, %s" cmp reg_values.(s1) reg_values.(s2));
                emit (Printf.sprintf "  %s = zext i1 %s to i64" v cmp);
                reg_values.(d) <- v
            
            | RLt (d, s1, s2) ->
                let cmp = fresh_var ctx in
                let v = fresh_var ctx in
                emit (Printf.sprintf "  %s = icmp slt i64 %s, %s" cmp reg_values.(s1) reg_values.(s2));
                emit (Printf.sprintf "  %s = zext i1 %s to i64" v cmp);
                reg_values.(d) <- v
            
            | RLe (d, s1, s2) ->
                let cmp = fresh_var ctx in
                let v = fresh_var ctx in
                emit (Printf.sprintf "  %s = icmp sle i64 %s, %s" cmp reg_values.(s1) reg_values.(s2));
                emit (Printf.sprintf "  %s = zext i1 %s to i64" v cmp);
                reg_values.(d) <- v
            
            | RGt (d, s1, s2) ->
                let cmp = fresh_var ctx in
                let v = fresh_var ctx in
                emit (Printf.sprintf "  %s = icmp sgt i64 %s, %s" cmp reg_values.(s1) reg_values.(s2));
                emit (Printf.sprintf "  %s = zext i1 %s to i64" v cmp);
                reg_values.(d) <- v
            
            | RGe (d, s1, s2) ->
                let cmp = fresh_var ctx in
                let v = fresh_var ctx in
                emit (Printf.sprintf "  %s = icmp sge i64 %s, %s" cmp reg_values.(s1) reg_values.(s2));
                emit (Printf.sprintf "  %s = zext i1 %s to i64" v cmp);
                reg_values.(d) <- v
            
            | RAnd (d, s1, s2) ->
                let v = fresh_var ctx in
                emit (Printf.sprintf "  %s = and i64 %s, %s" v reg_values.(s1) reg_values.(s2));
                reg_values.(d) <- v
            
            | ROr (d, s1, s2) ->
                let v = fresh_var ctx in
                emit (Printf.sprintf "  %s = or i64 %s, %s" v reg_values.(s1) reg_values.(s2));
                reg_values.(d) <- v
            
            | RNot (d, s) ->
                let cmp = fresh_var ctx in
                let v = fresh_var ctx in
                emit (Printf.sprintf "  %s = icmp eq i64 %s, 0" cmp reg_values.(s));
                emit (Printf.sprintf "  %s = zext i1 %s to i64" v cmp);
                reg_values.(d) <- v
            
            | RJump offset ->
                emit (Printf.sprintf "  br label %%%s" block_labels.(pc + offset))
            
            | RJumpIfFalse (c, offset) ->
                let cmp = fresh_var ctx in
                let then_label = block_labels.(pc + 1) in
                let else_label = block_labels.(pc + offset) in
                emit (Printf.sprintf "  %s = icmp eq i64 %s, 0" cmp reg_values.(c));
                emit (Printf.sprintf "  br i1 %s, label %%%s, label %%%s" cmp then_label else_label)
            
            | RJumpIfTrue (c, offset) ->
                let cmp = fresh_var ctx in
                let then_label = block_labels.(pc + 1) in
                let else_label = block_labels.(pc + offset) in
                emit (Printf.sprintf "  %s = icmp ne i64 %s, 0" cmp reg_values.(c));
                emit (Printf.sprintf "  br i1 %s, label %%%s, label %%%s" cmp then_label else_label)
            
            | RReturn r ->
                emit (Printf.sprintf "  ret i64 %s" reg_values.(r))
            
            | RLoadFunc (d, f) ->
                let v = fresh_var ctx in
                let target_name = if f = 0 then "main" else Printf.sprintf "func_%d" f in
                emit (Printf.sprintf "  %s = bitcast i64 (i64)* @%s to i64" v target_name);
                reg_values.(d) <- v
            
            | RCall (d, f, args) ->
                let v = fresh_var ctx in
                let arg_strs = List.map (fun r -> Printf.sprintf "i64 %s"  reg_values.(r)) args in
                let args_str = String.concat ", " arg_strs in
                emit (Printf.sprintf "  %s = call i64 %s(i64 %s)" v reg_values.(f) args_str);
                reg_values.(d) <- v
            
            | RTailCall (f, args) ->
                let arg_strs = List.map (fun r -> Printf.sprintf "i64 %s"  reg_values.(r)) args in
                let args_str = String.concat ", " arg_strs in
                emit (Printf.sprintf "  %s = call i64 %s(i64 %s)" (fresh_var ctx) reg_values.(f) args_str);
                emit "  ret i64 0"
            
            | RPrint r ->
                emit (Printf.sprintf "  call void @print_int(i64 %s)" reg_values.(r))
            
            | RNop -> ()
            
            | _ ->
                emit (Printf.sprintf "  ; TODO: %s" (string_of_reg_instr instr))
            );
            
            gen_instrs (pc + 1)
        end
    in
    
    gen_instrs 0;
    
    (* 如果最后一条指令不是 return，添加隐式 return *)
    (match Array.length func.code with
    | 0 -> emit "  ret i64 0"
    | n ->
        match func.code.(n - 1) with
        | RReturn _ -> ()
        | _ -> emit "  ret i64 0");
    
    emit "}";
    Buffer.contents buf

(** 生成完整的 LLVM IR 程序 *)
let generate_llvm_ir prog =
    let ctx = create_ctx () in
    let buf = Buffer.create 8192 in
    
    (* 声明外部函数 *)
    Buffer.add_string buf "; LLVM IR generated from MyLang register bytecode\n\n";
    Buffer.add_string buf "declare void @print_int(i64)\n\n";
    
    (* 生成所有函数 *)
    Array.iteri (fun idx func ->
        Buffer.add_string buf (generate_function ctx func idx);
        Buffer.add_string buf "\n"
    ) prog.functions;
    
    Buffer.contents buf
