(** 寄存器虚拟机执行器

    执行基于寄存器的字节码。
*)

open Core
open Reg_bytecode

exception RegVMError of string
exception RegReturn of reg_value

(** 执行寄存器程序 *)
let execute prog =
  let rec run_func func_idx args =
    let func = prog.functions.(func_idx) in
    let regs = Array.create ~len:(func.max_regs + func.num_locals) (RVInt 0) in
    
    List.iteri args ~f:(fun i arg -> regs.(i) <- arg);
    
    let env = ref [] in
    let pc = ref 0 in
    
    let get_reg r = regs.(r) in
    let set_reg r v = regs.(r) <- v in
    
    let lookup_env x =
      match List.Assoc.find !env ~equal:String.equal x with
      | Some v -> v
      | None -> raise (RegVMError ("未绑定变量: " ^ x))
    in
    
    let set_env x v =
      env := List.Assoc.add !env ~equal:String.equal x v
    in
    
    let get_const idx =
      match func.constants.(idx) with
      | CPInt n -> RVInt n
      | CPBool b -> RVBool b
      | CPString s -> RVString s
      | CPUnit -> RVUnit
    in
    
    try
      while !pc < Array.length func.code do
        let instr = func.code.(!pc) in
        pc := !pc + 1;
        
        match instr with
        | RLoadConst (d, c) -> set_reg d (get_const c)
        | RLoadNil d -> set_reg d RVNil
        | RMove (d, s) -> set_reg d (get_reg s)
        
        | RAdd (d, s1, s2) ->
            (match get_reg s1, get_reg s2 with
             | RVInt a, RVInt b -> set_reg d (RVInt (a + b))
             | RVString a, RVString b -> set_reg d (RVString (a ^ b))
             | _ -> raise (RegVMError "add: 类型错误"))
        
        | RSub (d, s1, s2) ->
            (match get_reg s1, get_reg s2 with
             | RVInt a, RVInt b -> set_reg d (RVInt (a - b))
             | _ -> raise (RegVMError "sub: 需要整数"))
        
        | RMul (d, s1, s2) ->
            (match get_reg s1, get_reg s2 with
             | RVInt a, RVInt b -> set_reg d (RVInt (a * b))
             | _ -> raise (RegVMError "mul: 需要整数"))
        
        | RDiv (d, s1, s2) ->
            (match get_reg s1, get_reg s2 with
             | RVInt _, RVInt 0 -> raise (RegVMError "除零错误")
             | RVInt a, RVInt b -> set_reg d (RVInt (a / b))
             | _ -> raise (RegVMError "div: 需要整数"))
        
        | REq (d, s1, s2) ->
             (match get_reg s1, get_reg s2 with
             | RVInt a, RVInt b -> set_reg d (RVBool (Int.equal a b))
             | RVBool a, RVBool b -> set_reg d (RVBool (Bool.equal a b))
             | RVString a, RVString b -> set_reg d (RVBool (String.equal a b))
             | _ -> raise (RegVMError "eq: 类型不匹配"))
        
        | RNeq (d, s1, s2) ->
             (match get_reg s1, get_reg s2 with
             | RVInt a, RVInt b -> set_reg d (RVBool (not (Int.equal a b)))
             | RVBool a, RVBool b -> set_reg d (RVBool (not (Bool.equal a b)))
             | _ -> raise (RegVMError "neq: 类型不匹配"))
        
        | RLt (d, s1, s2) ->
            (match get_reg s1, get_reg s2 with
             | RVInt a, RVInt b -> set_reg d (RVBool (a < b))
             | _ -> raise (RegVMError "lt: 需要整数"))
        
        | RLe (d, s1, s2) ->
            (match get_reg s1, get_reg s2 with
             | RVInt a, RVInt b -> set_reg d (RVBool (a <= b))
             | _ -> raise (RegVMError "le: 需要整数"))
        
        | RGt (d, s1, s2) ->
            (match get_reg s1, get_reg s2 with
             | RVInt a, RVInt b -> set_reg d (RVBool (a > b))
             | _ -> raise (RegVMError "gt: 需要整数"))
        
        | RGe (d, s1, s2) ->
            (match get_reg s1, get_reg s2 with
             | RVInt a, RVInt b -> set_reg d (RVBool (a >= b))
             | _ -> raise (RegVMError "ge: 需要整数"))
        
        | RAnd (d, s1, s2) ->
            (match get_reg s1, get_reg s2 with
             | RVBool a, RVBool b -> set_reg d (RVBool (a && b))
             | _ -> raise (RegVMError "and: 需要布尔值"))
        
        | ROr (d, s1, s2) ->
            (match get_reg s1, get_reg s2 with
             | RVBool a, RVBool b -> set_reg d (RVBool (a || b))
             | _ -> raise (RegVMError "or: 需要布尔值"))
        
        | RNot (d, s) ->
            (match get_reg s with
             | RVBool b -> set_reg d (RVBool (not b))
             | _ -> raise (RegVMError "not: 需要布尔值"))
        
        | RLoadVar (d, name) -> set_reg d (lookup_env name)
        | RStoreVar (name, s) -> set_env name (get_reg s)
        
        | RJump offset -> pc := !pc + offset - 1
        
        | RJumpIfFalse (c, offset) ->
            (match get_reg c with
             | RVBool false -> pc := !pc + offset - 1
             | RVBool true -> ()
             | _ -> raise (RegVMError "jump_if_false: 需要布尔值"))
        
        | RJumpIfTrue (c, offset) ->
            (match get_reg c with
             | RVBool true -> pc := !pc + offset - 1
             | RVBool false -> ()
             | _ -> raise (RegVMError "jump_if_true: 需要布尔值"))
        
        | RLoadFunc (d, f) -> set_reg d (RVClosure (f, !env))
        
        | RCall (d, f, arg_regs) ->
            let args = List.map arg_regs ~f:get_reg in
            (match get_reg f with
             | RVClosure (f_idx, _) ->
                 let result = run_func f_idx args in
                 set_reg d result
             | _ -> raise (RegVMError "call: 不是函数"))
        
        | RReturn r -> raise (RegReturn (get_reg r))
        
        | RPrint r ->
            (match get_reg r with
             | RVInt n -> print_endline (string_of_int n)
             | RVBool b -> print_endline (string_of_bool b)
             | RVString s -> print_endline s
             | RVUnit -> print_endline "()"
             | RVNil -> print_endline "nil"
             | RVClosure _ -> print_endline "<closure>")
        
        | RNop -> ()
        
        | _ -> raise (RegVMError ("未实现指令: " ^ string_of_reg_instr instr))
      done;
      RVUnit
    with
    | RegReturn v -> v
  in
  
  run_func prog.entry_point []
